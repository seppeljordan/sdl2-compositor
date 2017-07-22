module SDL.Compositor.Manipulator where

-- | This class models a graphics object that supports color
-- modulation.
class Manipulator m where
  -- | Modulate the alpha channel of picture.  This behavior stacks
  -- multiplicatively.
  modulateAlphaM :: Int -> m -> m
  -- | Modulate the red channel of picture.  This behavior stacks
  -- multiplicatively.
  modulateRedM :: Int -> m -> m
  -- | Modulate the green channel of picture.  This behavior stacks
  -- multiplicatively.
  modulateGreenM :: Int -> m -> m
  -- | Modulate the blue channel of picture.  This behavior stacks
  -- multiplicatively.
  modulateBlueM :: Int -> m -> m
