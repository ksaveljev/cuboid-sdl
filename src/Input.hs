{-# LANGUAGE Arrows #-}

module Input where

import FRP.Yampa (SF, returnA)
import qualified Graphics.UI.SDL as SDL

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import Types

pollEvents :: IO Events
pollEvents = alloca $ \pointer -> pollEvents' [] pointer
  where
    pollEvents' acc pointer = do
      status <- SDL.pollEvent pointer

      if status == 1
        then do
          ev <- peek pointer
          pollEvents' (ev:acc) pointer
        else return acc

parseInput :: SF Events ParsedInput
parseInput = proc events -> do
  returnA -< ParsedInput (fromIntegral $ length events) 0 0 0
