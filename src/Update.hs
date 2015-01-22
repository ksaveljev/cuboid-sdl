{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}

module Update where

import FRP.Yampa (SF, returnA)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..))

import Types

update :: SF ParsedInput GameState
update = proc pi@(ParsedInput{ wCount, aCount, sCount, dCount }) -> do
  returnA -< Game {level = Level (P3D wCount 0 0) (P3D 0 0 0) [], rotX = 0, playerPos = Vector3 0 0 0}
