module SDL.Compositor.Blender where

import SDL

-- | This class modells a graphics object that supports switching of
-- `BlendMode`.
class Blender b where
  -- | This method sets the `BlendMode` of an object.  If the object
  -- has children, their `BlendMode` property will be preserved.
  blendMode :: BlendMode -> b -> b
