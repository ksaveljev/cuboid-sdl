module Input where

import FRP.Yampa (SF)

import Types

pollEvents :: IO Events
pollEvents = return [] -- TODO: implement this!

parseInput :: SF Events ParsedInput
parseInput = undefined
