module SDL.Compositor.TTF
       ( -- * Interface
         FontSupport(..)
       , Alignment(..)
         -- * Texture generation
       , ColorWrapper(..)
       , FontKey(..)
       , defaultFontKey
       , textureFromKey
       )
where

import Control.Monad
import Data.Text
import SDL.TTF
import SDL.TTF.FFI (TTFFont)
import SDL.TTF.Types
import SDL
import SDL.Raw.Types (Color(..))

import SDL.Data.Cache

data Alignment = AlignTopLeft    | AlignTopRight    | AlignTopCenter
               | AlignLeft       | AlignRight       | AlignCenter
               | AlignBottomLeft | AlignBottomRight | AlignBottomCenter
               deriving (Eq,Read,Show)

newtype ColorWrapper = ColorWrapper Color deriving Eq

instance Ord ColorWrapper where
  compare (ColorWrapper (Color r1 g1 b1 a1))
          (ColorWrapper (Color r2 g2 b2 a2)) =
    compare (r1,g1,b1,a1) (r2,g2,b2,a2)

data FontKey = FontKey { fkStyle :: TTFStyle
                       , fkFont :: TTFFont
                       , fkHinting :: TTFHinting
                       , fkKerning :: Bool
                       , fkMessage :: Text
                       , fkColor :: ColorWrapper
                       }
             deriving (Eq,Ord)

defaultFontKey :: TTFFont -> FontKey
defaultFontKey font =
  FontKey { fkStyle = TTFNormal
          , fkFont = font
          , fkHinting = TTFHNormal
          , fkKerning = True
          , fkMessage = pack ""
          , fkColor = ColorWrapper (Color 255 255 255 255)
          }

class FontSupport c where
  withFontStyle :: TTFStyle -> c a -> c a
  withFont :: TTFFont -> c a -> c a
  withFontHint :: TTFHinting -> c a -> c a
  showText :: Alignment -> Color -> Text -> c a
  withKerning :: Bool -> c a -> c a
  withFontCache :: (Cacheable a) => Cache FontKey a -> c a -> c a

textureFromKey :: Renderer -> FontKey -> IO Texture
textureFromKey rend (FontKey style font
                     hints kerning msg (ColorWrapper color)) = do
  oldStyle <- getFontStyle font
  oldHinting <- getFontHinting font
  let changeStyle = oldStyle /= style
      changeHinting = oldHinting /= hints
  when changeStyle $ setFontStyle font style
  when changeHinting $ setFontHinting font hints
  if kerning
    then setFontKerning font KerningOn
    else setFontKerning font KerningOff
  surf <- renderUTF8Solid font (unpack msg) color
  tex <- createTextureFromSurface rend surf
  freeSurface surf
  when changeStyle $ setFontStyle font oldStyle
  when changeHinting $ setFontHinting font oldHinting
  return tex
