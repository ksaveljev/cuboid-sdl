module Types where

import Graphics.Rendering.OpenGL.Raw.Types (GLdouble)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3)
import qualified Graphics.UI.SDL as SDL

type Events = [SDL.Event]

type R = GLdouble

data Point3D = P3D { x :: Integer
                   , y :: Integer
                   , z :: Integer
                   } deriving (Show)

data Level = Level { startingPoint :: Point3D
                   , endPoint :: Point3D
                   , obstacles :: [Point3D]
                   } deriving (Show)

data GameState = Game { level :: Level
                      , rotX :: R
                      , playerPos :: Vector3 R
                      } deriving (Show)

data ParsedInput = ParsedInput { wCount :: Integer
                               , aCount :: Integer
                               , sCount :: Integer
                               , dCount :: Integer
                               }
