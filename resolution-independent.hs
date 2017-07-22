
import Data.Text (pack)
import Linear.V2 (V2(..))
import SDL (initialize, InitFlag(InitVideo), createWindow,
            WindowConfig(windowInitialSize), defaultWindow,
            createRenderer, defaultRenderer, clear, present,
            RendererConfig(rendererTargetTexture,rendererType),
            quit,($=), rendererLogicalSize, Texture,
            RendererType(SoftwareRenderer)
           )
import System.Environment (getArgs)
import SDL.Compositor.ResIndependent (translateR, filledRectangleR,
                                      fromRelativeCompositor, ResIndependent)
import SDL.Compositor (runRenderer, rgba, CompositingNode)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  -- Initialize sdl
  initialize [InitVideo]
  -- read window dimensions from command line args
  dims <- (\(w:h:_) -> V2 (read w) (read h)) <$> getArgs
  -- create a window with the given dimensions
  window <- createWindow (pack "test window") (defaultWindow {windowInitialSize = dims})
  -- create a renderer for the windows surface, software for
  -- compatibility reasons
  renderer <- createRenderer window (-1)
              (defaultRenderer {rendererTargetTexture = True
                               ,rendererType = SoftwareRenderer})
  -- setting the logical size is optional
  rendererLogicalSize renderer $= Just dims
  -- clear the window
  clear renderer
  -- draw a white square with length 0.99 for the edges
  let square :: ResIndependent CompositingNode Texture
      square = filledRectangleR (V2 0.99 0.99) (rgba 255 255 255 255)
  -- draw everything
  runRenderer
    renderer
    ( fromRelativeCompositor (fromIntegral <$> dims) $
      translateR (V2 0.5 0.5) -- move the square to the center of
      square
    )
  -- present what we have drawn to the user
  present renderer
  -- wait for 5 seconds
  threadDelay (5 * 10^(6::Int))
  -- quit sdl
  quit
