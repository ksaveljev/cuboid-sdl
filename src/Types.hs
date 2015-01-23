module Types where

import FRP.Yampa.Vector3
import Graphics.Rendering.OpenGL.Raw.Types (GLdouble)
import qualified Graphics.UI.SDL as SDL

type Events = [SDL.Event]

data Key = W | A | S | D | UP | DOWN | LEFT | RIGHT | OTHER deriving (Eq)

data WinLose = Win | Lose deriving (Eq)

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

data ParsedInput = ParsedInput { wCount :: Double
                               , aCount :: Double
                               , sCount :: Double
                               , dCount :: Double
                               , upEvents :: Int
                               , downEvents :: Int
                               , rightEvents :: Int
                               , leftEvents :: Int
                               , sdlQuit :: Bool
                               }
