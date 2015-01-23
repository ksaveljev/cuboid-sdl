module Graphics where

import FRP.Yampa.Vector3
import Control.Monad (void)
import qualified Graphics.UI.SDL as SDL

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))

import qualified Graphics.UI.GLUT as GLUT

import Foreign.C.String (withCAString)
import Foreign.C.Types (CInt)

import Types
import Utils

-- Helpful OpenGL constants for rotation
xAxis :: GL.Vector3 R
xAxis = GL.Vector3 1 0 0 :: GL.Vector3 R

yAxis :: GL.Vector3 R
yAxis = GL.Vector3 0 1 0 :: GL.Vector3 R

zAxis :: GL.Vector3 R
zAxis = GL.Vector3 0 0 1 :: GL.Vector3 R



initSDL :: IO ()
initSDL = void $ SDL.init SDL.SDL_INIT_VIDEO

quitSDL :: IO ()
quitSDL = SDL.quit

initGL :: IO ()
initGL = do
    void $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MAJOR_VERSION 3
    void $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MINOR_VERSION 2
    void $ SDL.glSetAttribute SDL.SDL_GL_DOUBLEBUFFER 1
    --void $ SDL.glSetAttribute SDL.SDL_GL_DEPTH_SIZE 24

    GL.depthFunc            $= Just GL.Less
    GL.clearColor           $= GL.Color4 0 0 0 0
    GL.light (GL.Light 0)   $= GL.Enabled
    GL.lighting             $= GL.Enabled
    GL.lightModelAmbient    $= GL.Color4 0.5 0.5 0.5 1
    GL.diffuse (GL.Light 0) $= GL.Color4 1 1 1 1
    GL.blend                $= GL.Enabled
    GL.blendFunc            $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.colorMaterial        $= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)

    return ()

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
draw window gameState = do
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
    renderGame gameState
    SDL.glSwapWindow window

renderGame :: GameState -> IO ()
renderGame (Game lvl rotationX pPos) = do
    GL.loadIdentity
    GL.translate $ GL.Vector3 (0 :: R) 0 (-2 * fromInteger (size lvl))
    -- TODO: calculate rotation axis based on rotX/Y
    GL.rotate (rotationX * 10) xAxis
    GL.color $ GL.Color3 (1 :: R) 1 1
    GL.position (GL.Light 0) $= GL.Vertex4 0 0 0 1
    GLUT.renderObject GLUT.Wireframe (GLUT.Cube $ fromInteger $ size lvl)
    renderPlayer pPos
    renderGoal (p3DtoV3 $ endPoint lvl)
    mapM_ (renderObstacle . p3DtoV3) $ obstacles lvl
    where size2 :: R
          size2 = fromInteger (size lvl) / 2
          green  = GL.Color4 0.8 1.0 0.7 0.9 :: GL.Color4 R
          greenG = GL.Color4 0.8 1.0 0.7 1.0 :: GL.Color4 R
          red    = GL.Color4 1.0 0.7 0.8 1.0 :: GL.Color4 R
          renderShapeAt s p = GL.preservingMatrix $ do
            GL.translate $ GL.Vector3 (0.5 - size2 + vector3X p)
                                      (0.5 - size2 + vector3Y p)
                                      (0.5 - size2 + vector3Z p)
            GLUT.renderObject GLUT.Solid s
          renderObstacle = (GL.color green >>) . (renderShapeAt $ GLUT.Cube 1)
          renderPlayer   = (GL.color red >>) . (renderShapeAt $ GLUT.Sphere' 0.5 20 20)
          renderGoal     = (GL.color greenG >>) . (renderShapeAt $ GLUT.Sphere' 0.5 20 20)
