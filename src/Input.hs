{-# LANGUAGE Arrows #-}

module Input where

import FRP.Yampa (SF, returnA, arr)
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
  quit <- isSDLQuitEventPresent -< events
  returnA -< ParsedInput (fromIntegral $ length events) 0 0 0 quit

isSDLQuitEventPresent :: SF Events Bool
isSDLQuitEventPresent = arr $ any isQuitEvent
  where isQuitEvent (SDL.QuitEvent _ _) = True
        isQuitEvent _ = False

getKey :: SDL.Keysym -> Key
getKey keysym = case SDL.keysymScancode keysym of
                  26 -> W
                  4  -> A
                  22 -> S
                  7  -> D
                  _  -> OTHER
