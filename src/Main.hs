{-# LANGUAGE Arrows #-}

import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Exception (bracket_)
import FRP.Yampa (reactimate, SF, returnA)
import qualified Graphics.UI.SDL as SDL

import Foreign.C.Types (CInt)

import Types
import Input
import Graphics

windowTitle :: String
windowTitle = "Cuboid"

windowWidth :: CInt
windowWidth = 640

windowHeight :: CInt
windowHeight = 480

-- TODO: actually implement this!
mainSF :: SF Events (IO ())
mainSF = proc _ ->
  returnA -< return ()

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
      actuate _ _ = return False -- TODO: implement this!

  reactimate init' sense actuate mainSF

  destroyRenderer renderer
  destroyWindow window
