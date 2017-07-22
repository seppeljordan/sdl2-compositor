{-# LANGUAGE MultiParamTypeClasses #-}
module SDL.Data.Texture
       ( Renderable(..)
       , Texture(..)
       , Renderer(..)
         -- * Bindings for low level sdl2
       , RawTexture(..)
       )
       where

import           Control.Lens (view)
import           Control.Monad (void)
import           Data.Bits ((.|.))
import           Data.StateVar (StateVar, makeStateVar)
import           Data.Word (Word8,Word32)
import           Foreign.C (CDouble(..),CInt)
import           Foreign.Marshal.Alloc (malloc, free)
import           Foreign.Marshal.Utils (with, maybeWith)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (Storable(peek))
import           Linear.Affine (Point(..))
import           Linear.V2 (V2(..), _x, _y)
import           Linear.V3 (V3(..))
import           Linear.V4 (V4)
import qualified SDL
import qualified SDL.Internal.Types as SDL
import qualified SDL.Raw as Raw

-- | This class modells that something can be rendered to another
-- thing.
class Renderable rend tex where
  copyEx :: rend                      -- ^ rendering context
         -> tex                       -- ^ texture
         -> Maybe (SDL.Rectangle Int) -- ^ source rectangle
         -> Maybe (SDL.Rectangle Int) -- ^ destination rectangle
         -> Double                    -- ^ rotation
         -> Maybe (Point V2 Int)      -- ^ rotation center
         -> V2 Bool                   -- ^ flipping
         -> IO ()
  createTexture :: rend
                -> SDL.PixelFormat
                -> SDL.TextureAccess
                -> V2 Int
                -> IO tex
  rendererRenderTarget :: rend -> StateVar (Maybe tex)

instance Renderable SDL.Renderer SDL.Texture where
  copyEx rend tex sourceRect destRect rot center flipping = void $
    SDL.copyEx rend tex (fmap fromIntegral <$> sourceRect)
    (fmap fromIntegral <$> destRect) (CDouble rot)
    (fmap fromIntegral <$> center) flipping
  createTexture r pf ta = SDL.createTexture r pf ta . fmap fromIntegral
  rendererRenderTarget = SDL.rendererRenderTarget

instance Renderable SDL.Renderer RawTexture where
  copyEx (SDL.Renderer rend) (RawTexture tex) sourceRect destRect rot center flipping =
    maybeWith with (toRawRect <$> sourceRect) $ \sourceRectPtr ->
    maybeWith with (toRawRect <$> destRect) $ \destRectPtr ->
    maybeWith with (toPoint <$> center) $ \centerPtr ->
    void $ Raw.renderCopyEx rend tex
    sourceRectPtr destRectPtr (CDouble rot) centerPtr (flipToWord flipping)
  rendererRenderTarget (SDL.Renderer rend) =
    makeStateVar getter setter
    where
      getter = do
        tex <- Raw.getRenderTarget rend
        if tex == nullPtr
          then return Nothing
          else (return . Just . RawTexture) tex
      setter Nothing = void $ Raw.setRenderTarget rend nullPtr
      setter (Just (RawTexture tex)) = void $ Raw.setRenderTarget rend tex
  createTexture (SDL.Renderer rend) format access (V2 w h) =
    RawTexture <$>
    Raw.createTexture rend (formatToRaw format) (accessToRaw access)
    (fromIntegral w) (fromIntegral h)

class Texture tex where
  textureAlphaMod :: tex -> StateVar Word8
  textureColorMod :: tex -> StateVar (V3 Word8)
  textureBlendMode :: tex -> StateVar SDL.BlendMode
  textureWidth :: tex -> IO Int
  textureHeight :: tex -> IO Int
  textureDims :: tex -> IO (V2 Int)
  textureDims t =
    V2 <$> textureWidth t <*> textureHeight t
  destroyTexture :: tex -> IO ()

instance Texture SDL.Texture where
  textureAlphaMod = SDL.textureAlphaMod
  textureColorMod = SDL.textureColorMod
  textureBlendMode = SDL.textureBlendMode
  textureWidth = fmap (fromIntegral . SDL.textureWidth) . SDL.queryTexture
  textureHeight = fmap (fromIntegral . SDL.textureHeight) . SDL.queryTexture
  textureDims = fmap (\q -> fromIntegral <$>
                            V2 (SDL.textureWidth q) (SDL.textureHeight q)) .
                SDL.queryTexture
  destroyTexture = SDL.destroyTexture

class Renderer rend where
  rendererDrawColor :: rend -> StateVar (V4 Word8)
  clear :: rend -> IO ()
  present :: rend -> IO ()
  drawRect :: rend -> Maybe (SDL.Rectangle Int) -> IO ()
  fillRect :: rend -> Maybe (SDL.Rectangle Int) -> IO ()
  drawLine :: rend -> Point V2 Int -> Point V2 Int -> IO ()

instance Renderer SDL.Renderer where
  rendererDrawColor = SDL.rendererDrawColor
  clear = SDL.clear
  present = SDL.present
  drawRect r = SDL.drawRect r . fmap (fmap fromIntegral)
  fillRect r = SDL.fillRect r . fmap (fmap fromIntegral)
  drawLine r a b = SDL.drawLine r (fromIntegral <$> a) (fromIntegral <$> b)

-- | Wrapper around the raw Texture type from SDL package.
newtype RawTexture = RawTexture { getRawTexture :: Raw.Texture }
                   deriving (Eq,Ord)

instance Texture RawTexture where
  textureAlphaMod (RawTexture rawTex) =
    makeStateVar getter setter
    where
      getter = returnSecond Raw.getTextureAlphaMod rawTex
      setter newMod = void $ Raw.setTextureAlphaMod rawTex newMod
  textureColorMod (RawTexture rawTex) =
    makeStateVar getter setter
    where
      getter = do
        rPtr <- malloc
        gPtr <- malloc
        bPtr <- malloc
        void $ Raw.getTextureColorMod rawTex rPtr gPtr bPtr
        colorVal <- V3 <$> peek rPtr <*> peek gPtr <*> peek bPtr
        free rPtr
        free gPtr
        free bPtr
        return colorVal
      setter (V3 rmod gmod bmod) =
        void $ Raw.setTextureColorMod rawTex rmod gmod bmod
  textureBlendMode (RawTexture rawTex) =
    makeStateVar getter setter
    where
      getter = convertBlendModeFromRaw <$>
               returnSecond Raw.getTextureBlendMode rawTex
      setter bm = void $ Raw.setTextureBlendMode rawTex
                  (convertBlendModeToRaw bm)
  textureDims (RawTexture rawTex) =
    fmap fromIntegral <$> queryTexture rawTex
  textureWidth (RawTexture rawTex) =
    fromIntegral . view _x <$> queryTexture rawTex
  textureHeight (RawTexture rawTex) =
    fromIntegral . view _y <$> queryTexture rawTex
  destroyTexture = Raw.destroyTexture . getRawTexture

returnSecond :: (Storable b) => (a -> Ptr b -> IO c) -> a -> IO b
returnSecond fun x = do
  ptr <- malloc
  _ <- fun x ptr
  val <- peek ptr
  free ptr
  return val

convertBlendModeFromRaw :: Raw.BlendMode -> SDL.BlendMode
convertBlendModeFromRaw bm = case bm of
    Raw.SDL_BLENDMODE_ADD -> SDL.BlendAdditive
    Raw.SDL_BLENDMODE_BLEND -> SDL.BlendAlphaBlend
    Raw.SDL_BLENDMODE_NONE -> SDL.BlendNone
    Raw.SDL_BLENDMODE_MOD -> SDL.BlendMod
    _ -> error $ "fromNumber<BlendMode>: unknown blend mode: " ++ show bm

convertBlendModeToRaw :: SDL.BlendMode -> Raw.BlendMode
convertBlendModeToRaw SDL.BlendNone = Raw.SDL_BLENDMODE_NONE
convertBlendModeToRaw SDL.BlendAlphaBlend = Raw.SDL_BLENDMODE_BLEND
convertBlendModeToRaw SDL.BlendAdditive = Raw.SDL_BLENDMODE_ADD
convertBlendModeToRaw SDL.BlendMod = Raw.SDL_BLENDMODE_MOD

toRawRect :: (Integral a) => SDL.Rectangle a -> Raw.Rect
toRawRect (SDL.Rectangle (P (V2 x y)) (V2 w h)) =
  Raw.Rect
  (fromIntegral x)
  (fromIntegral y)
  (fromIntegral w)
  (fromIntegral h)

toPoint :: (Integral a) => Point V2 a -> Raw.Point
toPoint (P (V2 x y)) = Raw.Point (fromIntegral x) (fromIntegral y)

flipToWord :: V2 Bool -> Raw.RendererFlip
flipToWord (V2 h v)
  | h && v =
      Raw.SDL_FLIP_VERTICAL .|. Raw.SDL_FLIP_HORIZONTAL
  | h = Raw.SDL_FLIP_HORIZONTAL
  | v = Raw.SDL_FLIP_VERTICAL
  | otherwise = Raw.SDL_FLIP_NONE

formatToRaw :: SDL.PixelFormat -> Word32
formatToRaw pf = case pf of
  SDL.Unknown -> Raw.SDL_PIXELFORMAT_UNKNOWN
  SDL.Index1LSB -> Raw.SDL_PIXELFORMAT_INDEX1LSB
  SDL.Index1MSB -> Raw.SDL_PIXELFORMAT_INDEX1MSB
  SDL.Index4LSB -> Raw.SDL_PIXELFORMAT_INDEX4LSB
  SDL.Index4MSB -> Raw.SDL_PIXELFORMAT_INDEX4MSB
  SDL.Index8 -> Raw.SDL_PIXELFORMAT_INDEX8
  SDL.RGB332 -> Raw.SDL_PIXELFORMAT_RGB332
  SDL.RGB444 -> Raw.SDL_PIXELFORMAT_RGB444
  SDL.RGB555 -> Raw.SDL_PIXELFORMAT_RGB555
  SDL.BGR555 -> Raw.SDL_PIXELFORMAT_BGR555
  SDL.ARGB4444 -> Raw.SDL_PIXELFORMAT_ARGB4444
  SDL.RGBA4444 -> Raw.SDL_PIXELFORMAT_RGBA4444
  SDL.ABGR4444 -> Raw.SDL_PIXELFORMAT_ABGR4444
  SDL.BGRA4444 -> Raw.SDL_PIXELFORMAT_BGRA4444
  SDL.ARGB1555 -> Raw.SDL_PIXELFORMAT_ARGB1555
  SDL.RGBA5551 -> Raw.SDL_PIXELFORMAT_RGBA5551
  SDL.ABGR1555 -> Raw.SDL_PIXELFORMAT_ABGR1555
  SDL.BGRA5551 -> Raw.SDL_PIXELFORMAT_BGRA5551
  SDL.RGB565 -> Raw.SDL_PIXELFORMAT_RGB565
  SDL.BGR565 -> Raw.SDL_PIXELFORMAT_BGR565
  SDL.RGB24 -> Raw.SDL_PIXELFORMAT_RGB24
  SDL.BGR24 -> Raw.SDL_PIXELFORMAT_BGR24
  SDL.RGB888 -> Raw.SDL_PIXELFORMAT_RGB888
  SDL.RGBX8888 -> Raw.SDL_PIXELFORMAT_RGBX8888
  SDL.BGR888 -> Raw.SDL_PIXELFORMAT_BGR888
  SDL.BGRX8888 -> Raw.SDL_PIXELFORMAT_BGRX8888
  SDL.ARGB8888 -> Raw.SDL_PIXELFORMAT_ARGB8888
  SDL.RGBA8888 -> Raw.SDL_PIXELFORMAT_RGBA8888
  SDL.ABGR8888 -> Raw.SDL_PIXELFORMAT_ABGR8888
  SDL.BGRA8888 -> Raw.SDL_PIXELFORMAT_BGRA8888
  SDL.ARGB2101010 -> Raw.SDL_PIXELFORMAT_ARGB2101010
  SDL.YV12 -> Raw.SDL_PIXELFORMAT_YV12
  SDL.IYUV -> Raw.SDL_PIXELFORMAT_IYUV
  SDL.YUY2 -> Raw.SDL_PIXELFORMAT_YUY2
  SDL.UYVY -> Raw.SDL_PIXELFORMAT_UYVY
  SDL.YVYU -> Raw.SDL_PIXELFORMAT_YVYU

accessToRaw :: SDL.TextureAccess -> CInt
accessToRaw t = case t of
  SDL.TextureAccessStatic -> Raw.SDL_TEXTUREACCESS_STATIC
  SDL.TextureAccessStreaming -> Raw.SDL_TEXTUREACCESS_STREAMING
  SDL.TextureAccessTarget -> Raw.SDL_TEXTUREACCESS_TARGET

queryTexture :: Raw.Texture -> IO (V2 CInt)
queryTexture tex = do
  formatPtr <- malloc
  accessPtr <- malloc
  wPtr <- malloc
  hPtr <- malloc
  void $ Raw.queryTexture tex formatPtr accessPtr wPtr hPtr
  free accessPtr
  free formatPtr
  dims <- V2 <$> peek wPtr <*> peek hPtr
  mapM_ free [wPtr, hPtr]
  return dims
