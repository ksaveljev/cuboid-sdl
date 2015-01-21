import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Exception (bracket_)
import FRP.Yampa (reactimate, SF)
import qualified Graphics.UI.SDL as SDL

import Types
import Input

mainSF :: SF Events (IO ())
mainSF = undefined

main :: IO ()
main = bracket_ (SDL.init SDL.SDL_INIT_VIDEO) SDL.quit $ do
  timeRef <- newIORef =<< SDL.getTicks

  let init' = SDL.getTicks >>= writeIORef timeRef >> return []
      sense _ = do currentTimeRef <- SDL.getTicks
                   previousTimeRef <- readIORef timeRef
                   writeIORef timeRef currentTimeRef
                   let dt = fromIntegral (currentTimeRef - previousTimeRef) / 1000
                   events <- pollEvents
                   return (dt, if null events then Nothing else Just events)
      actuate _ _ = undefined

  reactimate init' sense actuate mainSF
