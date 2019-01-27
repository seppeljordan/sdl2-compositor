{-# LANGUAGE ScopedTypeVariables #-}

-- Declarative image composition based on sdl2
-- Copyright (C) 2015  Sebastian Jordan
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | This module provides the means for declarative image generation
-- using sdl2 primitives as a basis.  Atomical operations for image
-- composition are rotation, translation, mirroring, color modulation,
-- changing blend modes and primitive drawing.
--
-- This packages aims to provide a basic interface via type classes.
-- This means that you could write your own implementation but still
-- use eventual utility functions provided by this package.  The
-- authors decided to split the functionality into several typeclasses
-- to allow partial implementations while preserving type safety.
module SDL.Compositor
    ( -- * Interface
      Compositor (..)
    , Blender (..)
    , Manipulator (..)
    , Drawer (..)
    , AbsoluteSize (..)
    , Texture(..)
    , Renderer(..)
    , Renderable(..)
    -- * Utility
    , withZIndex
    -- * Implementation
    , CompositingNode
    , runRenderer
    -- * Colors
    , Color
    , rgba
    )
where

import           Control.Lens (over, Lens, lens, view, set)
import           Control.Monad (when)
import           Data.List
import           Data.Maybe
import           Data.StateVar (get,($=))
import           Data.Word
import           Linear.Affine
import           Linear.V2
import           Linear.V3
import           Linear.V4
import qualified SDL

import           SDL.Compositor.Blender
import           SDL.Compositor.Drawer
import           SDL.Compositor.Manipulator
import           SDL.Data.Texture (Texture(..), Renderable(..), Renderer(..))

-- | A compositing node represents a compound graphical resource.
data CompositingNode a = Sized (V2 Int) a
                       | FilledRectangle (V2 Int) (V4 Word8)
                       | Rectangle (V2 Int) (V4 Word8)
                       | Line (V2 Int) (V4 Word8)
                       | AlphaMod Double (CompositingNode a)
                       | RedMod Double (CompositingNode a)
                       | GreenMod Double (CompositingNode a)
                       | BlueMod Double (CompositingNode a)
                       | PreserveBlendMode SDL.BlendMode (CompositingNode a)
                       | (CompositingNode a) `Under` (CompositingNode a)
                       | Flipped (V2 Bool) (CompositingNode a)
                       | Rotated Double (CompositingNode a)
                       | Translated (V2 Int) (CompositingNode a)
                       | NoOP
                       deriving (Show,Eq)

instance Manipulator (CompositingNode a) where
  modulateAlphaM _ NoOP = NoOP
  modulateAlphaM modulator node = AlphaMod (fromIntegral modulator) node
  modulateRedM _ NoOP = NoOP
  modulateRedM modulator node = RedMod (fromIntegral modulator) node
  modulateGreenM _ NoOP = NoOP
  modulateGreenM modulator node = GreenMod (fromIntegral modulator) node
  modulateBlueM _ NoOP = NoOP
  modulateBlueM modulator node = BlueMod (fromIntegral modulator) node

instance Blender (CompositingNode a) where
  blendMode _ NoOP = NoOP
  blendMode mode node = PreserveBlendMode mode node

instance Compositor (CompositingNode a) where
  node1 `overC` NoOP = node1
  NoOP `overC` node2 = node2
  node1 `overC` node2 = node2 `Under` node1
  rotateC = Rotated
  flipC _ NoOP = NoOP
  flipC f node = Flipped f node

instance AbsoluteSize CompositingNode where
  translateA _ NoOP = NoOP
  translateA v node = Translated v node
  sizedA = Sized

instance Drawer (CompositingNode a) where
  rectangleC dims = Rectangle dims . colorToVector
  filledRectangleC dims = FilledRectangle dims . colorToVector
  lineC dims = Line dims . colorToVector

instance Semigroup (CompositingNode a) where
  node1 <> node2 = node2 `overC` node1

-- | 'mempty' represents no painting at all. Also
--
-- prop> mappend a b == overC a b
instance Monoid (CompositingNode a) where
  mempty = NoOP

infixr 5 `overC`

-- | A Compositor is a thing that can overlap, rotate and mirror
-- objects.
class Compositor c where
  -- | @overC x y@ positions x over y.  The meaning of this depends on
  -- the context.  For Textures and drawings this means that x should
  -- be drawn after y was drawn.
  overC :: c -> c -> c
  rotateC :: Double -> c -> c
  -- | This function takes a 'V2 Bool' that represents mirroring
  -- action.  The first component of the vector represents mirroring
  -- along the y-axis (horizontally) and the second component
  -- represents mirroring along the x-axis (vertically).
  flipC :: V2 Bool -> c -> c

class AbsoluteSize c where
  translateA :: V2 Int -> c a -> c a
  sizedA :: V2 Int -> a -> c a

-- | Arrange all given compositions in one composition.
--
-- This function takes a list of pairs where the first element of the
-- pair is the z-index and the second element is the composition.
-- Elements of with a higher z-index will be rendered "in front of"
-- elements with lower indices.  If elements have the same index then
-- the element that comes first in the list will be drawn over all the
-- later ones.
--
-- This method can only arrange compositions that are in the "the same
-- list of arguments".  That means that
--
-- > withZIndex [(1,a),(2,b)] `overC` withZIndex [(3,c)]
--
-- will always result in @b@ being rendered "in front of" @a@ and @c@,
-- no matter how large the z-index of @c@ is.
withZIndex :: (Compositor c, Monoid c) =>
              [(Int,c)] -> c
withZIndex = go.map snd.sortOn (negate.fst) where
  go = foldl overC mempty

data RendState rend tex = RendState { _alphaMod :: Double
                                    , _redMod :: Double
                                    , _greenMod :: Double
                                    , _blueMod :: Double
                                    , _renderTarget :: rend
                                    , _translationVec :: V2 Double
                                    , _rotationAngle :: Double
                                    , _blendMode :: Maybe SDL.BlendMode
                                    , _flipping :: V2 Bool
                                    }

defaultState :: r -> RendState r t
defaultState target =
  RendState { _alphaMod = 255
            , _redMod = 255
            , _greenMod = 255
            , _blueMod = 255
            , _renderTarget = target
            , _translationVec = 0
            , _rotationAngle = 0
            , _blendMode = Nothing
            , _flipping = V2 False False
            }

alphaMod,redMod,greenMod,blueMod,rotationAngle
  :: Lens (RendState r t) (RendState r t) Double Double
alphaMod = lens _alphaMod (\st a -> st{_alphaMod = a})
redMod = lens _redMod (\st r -> st{_redMod=r})
greenMod = lens _greenMod (\st g -> st{_greenMod=g})
blueMod = lens _blueMod (\st b -> st{_blueMod=b})
rotationAngle = lens _rotationAngle (\st a -> st{_rotationAngle=a})

renderTarget :: Lens (RendState r t) (RendState r t) r r
renderTarget = lens _renderTarget (\st rt -> st{_renderTarget=rt})

flipping :: Lens (RendState r t) (RendState r t) (V2 Bool) (V2 Bool)
flipping = lens _flipping (\st f -> st{_flipping=f})

translationVec :: Lens (RendState r t) (RendState r t) (V2 Double) (V2 Double)
translationVec = lens _translationVec (\st tv -> st{_translationVec=tv})

bm :: Lens (RendState r t) (RendState r t) (Maybe SDL.BlendMode) (Maybe SDL.BlendMode)
bm = lens _blendMode (\st b -> st{_blendMode=b})

-- | Render a composed image.
runRenderer :: forall tex rend .
               (Texture tex, Renderer rend, Renderable rend tex) =>
               rend -> CompositingNode tex -> IO ()
runRenderer target node = do
  currentDrawColor <- SDL.get (rendererDrawColor target)
  renderNode (defaultState target) node
  rendererDrawColor target SDL.$= currentDrawColor

renderNode :: forall tex rend .
              (Texture tex, Renderable rend tex, Renderer rend) =>
              RendState rend tex
           -> CompositingNode tex
           -> IO ()
renderNode _ NoOP = return ()
renderNode env (AlphaMod m node) = renderNode (over alphaMod (*(m/255)) env) node
renderNode env (RedMod m node) = renderNode (over redMod (*(m/255)) env) node
renderNode env (GreenMod m node) = renderNode (over greenMod (*(m/255)) env) node
renderNode env (BlueMod m node) = renderNode (over blueMod (*(m/255)) env) node
renderNode env (Translated vec node) =
  let currentAngle = view rotationAngle env
      V2 horFlip verFlip = view flipping env
      rotatedVec = rotateV2 currentAngle (fromIntegral <$> vec)
      transVec = V2 (if horFlip then -1 else 1) (if verFlip then -1 else 1) * rotatedVec
  in renderNode (over translationVec (+ transVec) env) node
renderNode env (node1 `Under` node2) =
  renderNode env node1 >> renderNode env node2
renderNode env (Flipped f node) =
  renderNode (over flipping ((/=) <$> f <*>) env) node
renderNode env (Rotated ang node) =
  renderNode (set rotationAngle newAngle env) node
  where currentAngle = view rotationAngle env
        V2 horFlip verFlip = view flipping env
        newAngle = if horFlip /= verFlip
                   then currentAngle - ang
                   else currentAngle + ang
renderNode env (PreserveBlendMode mode node) =
  renderNode (set bm (Just mode) env) node
renderNode _ (Sized (V2 0 _) _) = return ()
renderNode _ (Sized (V2 _ 0) _) = return ()
renderNode env (Sized dims tex) = do
  let renderer = view renderTarget env
      midPoint = view translationVec env
      ang = (negate.(/pi).(*180)) $
            view rotationAngle env
      targetRect = round <$>
        SDL.Rectangle
        (P (midPoint - (fromIntegral <$> dims) / 2))
        (fromIntegral <$> dims)
  setColorsAndBlend env tex
  copyEx renderer tex Nothing (Just targetRect) ang
    Nothing (view flipping env)
renderNode env (Rectangle dims colors) = do
  let rend = view renderTarget env
  -- get old values
  oldTarget <- get $ rendererRenderTarget rend :: IO (Maybe tex)
  -- set new values
  tex <- createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fromIntegral <$> dims)
  rendererRenderTarget rend $= Just tex
  rendererDrawColor rend $= V4 0 0 0 0
  rendererDrawColor rend $= fromIntegral <$> colors
  drawRect rend (Just (SDL.Rectangle 0 (fromIntegral <$> dims)))
  rendererRenderTarget rend $= oldTarget
  renderNode env (Sized dims tex)
  destroyTexture tex
renderNode env (Line dims colors) = do
  let rend = view renderTarget env
      flippingVector = (\b -> if b then (-1) else 1) <$> view flipping env
  -- get old values
  oldTarget <- get $ rendererRenderTarget rend :: IO (Maybe tex)
  -- set new values
  tex <- createTexture
         rend
         SDL.RGBA8888
         SDL.TextureAccessTarget
         (dims*flippingVector)
  rendererRenderTarget rend $= Just tex
  rendererDrawColor rend $= V4 0 0 0 0
  rendererDrawColor rend $= fromIntegral <$> colors
  drawLine rend 0 (P $ fromIntegral <$> dims)
  rendererRenderTarget rend $= oldTarget
  -- render created texture
  renderNode env (Sized dims tex)
  destroyTexture tex
renderNode env (FilledRectangle dims colors) = do
  let rend = view renderTarget env
  -- get old values
  oldTarget <- get $ rendererRenderTarget rend :: IO (Maybe tex)
  -- set new values
  tex <- createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fromIntegral <$> dims)
  rendererRenderTarget rend $= Just tex
  rendererDrawColor rend $= V4 0 0 0 0
  rendererDrawColor rend $= fromIntegral <$> colors
  fillRect rend (Just (SDL.Rectangle 0 (fromIntegral <$> dims)))
  rendererRenderTarget rend $= oldTarget
  renderNode env (Sized dims tex)
  destroyTexture tex

getCurrentBlendMode :: RendState r t -> SDL.BlendMode
getCurrentBlendMode env =
  fromMaybe SDL.BlendNone $
  view bm env

setColorsAndBlend :: (Texture a) => RendState t a -> a -> IO ()
setColorsAndBlend env tex = do
  let safeToWord8 :: Int -> Word8
      safeToWord8 n | n > 255 = 255
                    | n < 0 = 0
                    | otherwise = fromIntegral n
      alpha = (safeToWord8 . round . view alphaMod) env
      red = (safeToWord8 . round . view redMod) env
      green = (safeToWord8 . round . view greenMod) env
      blue = (safeToWord8 . round . view blueMod ) env
      blend = getCurrentBlendMode env
      rgb = V3 red green blue
  oldAlpha <- SDL.get (textureAlphaMod tex)
  oldRGB <- SDL.get (textureColorMod tex)
  oldBlend <- SDL.get (textureBlendMode tex)
  when (oldAlpha /= alpha) $ textureAlphaMod tex $= alpha
  when (oldRGB /= rgb) $ textureColorMod tex $= rgb
  when (oldBlend /= blend) $ textureBlendMode tex $= blend

rotateV2 :: Double -> V2 Double -> V2 Double
rotateV2 ang (V2 x y) =
  V2 (x * cos ang - y * sin ang)
     (x * sin ang + y * cos ang)
