import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Exception (bracket_)
import Control.Category ((>>>))
import FRP.Yampa (reactimate)
import qualified Graphics.UI.SDL as SDL

import Foreign.C.Types (CInt)

import Input
import Update
import Graphics

windowTitle :: String
windowTitle = "Cuboid"

windowWidth :: CInt
windowWidth = 640

windowHeight :: CInt
windowHeight = 480

main :: IO ()
main = bracket_ initSDL quitSDL $ do
  timeRef <- newIORef =<< SDL.getTicks

  window <- createWindow windowTitle windowWidth windowHeight

  initGL

  renderer <- createRenderer window

  let init' = SDL.getTicks >>= writeIORef timeRef >> return []
      sense _ = do currentTimeRef <- SDL.getTicks
                   previousTimeRef <- readIORef timeRef
                   writeIORef timeRef currentTimeRef
                   let dt = fromIntegral (currentTimeRef - previousTimeRef) / 1000
                   events <- pollEvents
                   return (dt, if null events then Nothing else Just events)
      actuate _ gameState = draw gameState >> return False

  reactimate init' sense actuate (parseInput >>> update)

  destroyRenderer renderer
  destroyWindow window
