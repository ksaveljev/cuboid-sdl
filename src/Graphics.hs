module Graphics where

import Control.Monad (void)
import qualified Graphics.UI.SDL as SDL

import Foreign.C.String (withCAString)
import Foreign.C.Types (CInt)

import Types

initSDL :: IO ()
initSDL = void $ SDL.init SDL.SDL_INIT_VIDEO

quitSDL :: IO ()
quitSDL = SDL.quit

initGL :: IO ()
initGL = do
    void $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MAJOR_VERSION 3
    void $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MINOR_VERSION 2
    void $ SDL.glSetAttribute SDL.SDL_GL_DOUBLEBUFFER 1
    void $ SDL.glSetAttribute SDL.SDL_GL_DEPTH_SIZE 24

createWindow :: String -> CInt -> CInt -> IO SDL.Window
createWindow title width height =
    withCAString title $ \t ->
      SDL.createWindow t SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED width height SDL.SDL_WINDOW_OPENGL

destroyWindow :: SDL.Window -> IO ()
destroyWindow = SDL.destroyWindow

createRenderer :: SDL.Window -> IO SDL.GLContext
createRenderer = SDL.glCreateContext

destroyRenderer :: SDL.GLContext -> IO ()
destroyRenderer = SDL.glDeleteContext

draw :: SDL.Window -> GameState -> IO ()
draw window gameState = print gameState
