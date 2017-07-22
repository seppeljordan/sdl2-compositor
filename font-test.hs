import           Control.Concurrent
import           Control.Monad
import           Data.Text hiding (head)
import           Linear.V2
import           Linear.V4
import           SDL
  (quit, BlendMode(BlendAlphaBlend),defaultWindow,initializeAll,
   defaultRenderer,createWindow,createRenderer)
import           SDL.Compositor
  (present,clear,translateA,overC,blendMode,runRenderer,filledRectangleC)
import           SDL.Compositor.TTF (withFont,showText,Alignment(AlignLeft))
import           SDL.Raw (Color(..))
import qualified SDL.TTF as TTF
import           System.Environment

main = do
  void TTF.init
  initializeAll
  args <- getArgs
  let fontPath = head args
  font <- TTF.openFont fontPath 36
  window <- createWindow (pack "Testwindow") defaultWindow
  rend <- createRenderer window (-1) defaultRenderer
  let hello = withFont font $
              showText AlignLeft (Color 255 255 255 255) (pack "Hello, World!")
      rectangle = filledRectangleC (V2 400 200) (V4 255 0 0 255)
  clear rend
  runRenderer rend (translateA (V2 400 300) $
                    blendMode BlendAlphaBlend $
                    (hello `overC` rectangle))
  present rend
  threadDelay (5*1000000)
  TTF.quit
  quit
  
  
  
