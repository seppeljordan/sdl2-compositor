module SDL.Compositor.Drawer where

import Data.Word
import Linear.V2
import Linear.V4

class Drawer d where
  rectangleC :: V2 Int -> Color -> d
  lineC :: V2 Int -> Color -> d
  filledRectangleC :: V2 Int -> Color -> d

-- | Represents the value of color.
newtype Color = Color {unColor :: V4 Word8}

-- | construct a color value from red green blue and alpha values.
rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgba r g b a = Color $ V4 r g b a

-- | Convert a color value to vector of numbers that represent the
-- color.
colorToVector :: Color -> V4 Word8
colorToVector = unColor
