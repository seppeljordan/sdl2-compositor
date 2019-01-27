{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module provides an implementation for simple resolution
-- independent drawing and rendering.  You can do the same stuff with
-- the resolution independent compositor as with a regular one.
--
-- One important difference is that the 'translateA' function as well
-- as the standard drawing functions don't work with 'ResIndependent'.
-- Instead there are replacement functions for these operations.
--
-- This implementation finds the biggest square that fits into the
-- dimensions of the rendering surface.  The top left corner of this
-- sqare is @V2 0 0@, the bottom right corner is @V2 1 1@.

module SDL.Compositor.ResIndependent
    ( -- * Wrapping and unwrapping
      ResIndependent
    , fromRelativeCompositor
      -- * Drawing
    , rectangleR
    , filledRectangleR
    , lineR
      -- * Composition
    , RelativeSize (..)
    )
where

import Linear.V2

import SDL.Compositor

class RelativeSize c where
  -- | Translate a composition by a given resolution independent
  -- vector.
  translateR :: V2 Float -> c a -> c a
  -- | This function takes a rectangular area given as resolution
  -- independent coordinates and an object and constructs a
  -- compositing node from it that spans the given area.
  sizedR :: V2 Float -> a -> c a

newtype ResIndependent c a = ResIndependent (V2 Int -> c a)
                           deriving (Functor,Monoid,Semigroup)

instance Compositor (c a) => Compositor (ResIndependent c a) where
  overC (ResIndependent fun1) (ResIndependent fun2) =
    ResIndependent $ \dims -> fun1 dims `overC` fun2 dims
  rotateC ang (ResIndependent fun) = ResIndependent $ \dims -> rotateC ang (fun dims)
  flipC flipping (ResIndependent fun) = ResIndependent $ \dims -> flipC flipping (fun dims)

instance (AbsoluteSize c) => RelativeSize (ResIndependent c) where
  translateR coords (ResIndependent fun) = ResIndependent $ \dims ->
    translateA (scaleToFormat dims coords) (fun dims)
  sizedR texDims tex = ResIndependent $ \dims ->
    sizedA (scaleToFormat dims texDims) tex

instance Manipulator (c a) => Manipulator (ResIndependent c a) where
  modulateAlphaM val (ResIndependent fun) = ResIndependent $ \dims -> modulateAlphaM val (fun dims)
  modulateRedM val (ResIndependent fun) = ResIndependent $ \dims -> modulateRedM val (fun dims)
  modulateGreenM val (ResIndependent fun) = ResIndependent $ \dims -> modulateGreenM val (fun dims)
  modulateBlueM val (ResIndependent fun) = ResIndependent $ \dims -> modulateBlueM val (fun dims)

instance Blender (c a) => Blender (ResIndependent c a) where
  blendMode mode (ResIndependent fun) = ResIndependent $ \dims -> blendMode mode (fun dims)

scaleToFormat :: V2 Int -> V2 Float -> V2 Int
scaleToFormat (V2 w h) coords =
  round <$> scale * coords where
    scale = fromIntegral (min w h)

shiftToFormat :: V2 Int -> V2 Int -> V2 Int
shiftToFormat (V2 w h) (V2 x y)
  | w == h = V2 x y
  | w > h = let delta = (w - h) `div` 2
            in V2 (x + delta) y
  | otherwise = let delta = (h - w) `div` 2
                in V2 x (y + delta)

-- | Convert a resolution independent compositor into a compositor for
-- a fixed screen size by specifying the size of the screen.
fromRelativeCompositor :: (Compositor (c a), AbsoluteSize c) =>
                          V2 Int -> ResIndependent c a -> c a
fromRelativeCompositor dims (ResIndependent fun) = translateA (shiftToFormat dims 0) (fun dims)

rectangleR :: (Drawer (c a)) => V2 Float -> Color -> ResIndependent c a
rectangleR rectDims colors = ResIndependent $ \dims ->
  rectangleC (scaleToFormat dims rectDims) colors

filledRectangleR :: (Drawer (c a)) => V2 Float -> Color -> ResIndependent c a
filledRectangleR rectDims colors = ResIndependent $ \dims ->
  filledRectangleC (scaleToFormat dims rectDims) colors

lineR :: (Drawer (c a)) => V2 Float -> Color -> ResIndependent c a
lineR lineCoords colors = ResIndependent $ \dims ->
  lineC (scaleToFormat dims lineCoords) colors
