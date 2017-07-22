
import Control.Concurrent (threadDelay)
import Data.Text (pack)
import Linear.V2 (V2(..))
import SDL (initialize, InitFlag(InitVideo), createWindow, defaultWindow,
            createRenderer, RendererConfig(rendererTargetTexture),
            defaultRenderer, rendererLogicalSize, ($=), present, clear, quit,
            BlendMode(BlendAlphaBlend), Texture)
import SDL.Compositor (filledRectangleC, translateA, runRenderer, overC,
                       modulateAlphaM, blendMode, rgba, CompositingNode)

main :: IO ()
main = do
  -- intialize SDL
  initialize [InitVideo]
  -- create a window
  window <- createWindow (pack "test window") defaultWindow
  -- create a renderer for the window
  rend <- createRenderer window (-1) (defaultRenderer {rendererTargetTexture = True})
  -- set the logical size of the window to 800x600
  rendererLogicalSize rend $= Just (V2 800 600)
  -- clear the renderer
  clear rend
  let
    -- 3 rectangles, one in red, one in green and one in blue, width:
    -- 100, height 150
    rectRed, rectGreen, rectBlue :: CompositingNode Texture
    rectRed = filledRectangleC (V2 100 150) (rgba 255 100 100 255)
    rectGreen = filledRectangleC (V2 100 150) (rgba 100 255 100 255)
    rectBlue = filledRectangleC (V2 100 150) (rgba 100 100 255 255)
    -- translate an image by (250,300) pixels
    translateToCenter = translateA (V2 250 300)
  -- draw everything
  runRenderer rend
    ( ( -- enable alpha blending for the whole subtree
        blendMode BlendAlphaBlend .
        -- make all the images slightly transparent
        modulateAlphaM 150 .
        -- align the images in the center
        translateToCenter
      )
      ( -- red, green and blue
        rectRed `overC`
        translateA (V2 150 0) (rectGreen `overC`
                               translateA (V2 150 0) rectBlue)
      )
    )
  -- show the drawn image on the screen
  present rend
  -- pause for 5 seconds
  threadDelay (5 * (10::Int)^(6::Int))
  -- quit SDL
  quit
