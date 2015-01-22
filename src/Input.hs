{-# LANGUAGE Arrows #-}
{-# LANGUAGE PatternSynonyms #-}

module Input ( parseInput
             , pollEvents
             ) where

import FRP.Yampa (SF, returnA, arr, (^<<), (^>>), integral, (>>>))
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

keyIntegral :: Double -> SF Events Double
keyIntegral a = arr (sum . map (const a)) >>> integral

parseInput :: SF Events ParsedInput
parseInput = proc events -> do
  quit     <- isSDLQuitEventPresent      -< events
  down     <- filterKeyDowns             -< events
  w        <- countKey W                 -< down
  a        <- countKey A                 -< down
  s        <- countKey S                 -< down
  d        <- countKey D                 -< down
  upEvs    <- length ^<< filterKey UP    -< down
  downEvs  <- length ^<< filterKey DOWN  -< down
  rightEvs <- length ^<< filterKey RIGHT -< down
  leftEvs  <- length ^<< filterKey LEFT  -< down
  returnA -< ParsedInput w a s d upEvs downEvs rightEvs leftEvs quit

  where countKey c = filterKey c ^>> keyIntegral 1
        filterKey c = arr $ filter (isKey c)
          where isKey expected (SDL.KeyboardEvent _ _ _ _ _ keysym) | getKey keysym == expected = True
                isKey _ _ = False
        filterKeyDowns = arr $ filter isSDLKeyDown
          where isSDLKeyDown (SDL.KeyboardEvent SDL.SDL_KEYDOWN _ _ _ _ _) = True
                isSDLKeyDown _ = False


isSDLQuitEventPresent :: SF Events Bool
isSDLQuitEventPresent = arr $ any isQuitEvent
  where isQuitEvent (SDL.QuitEvent _ _) = True
        isQuitEvent _ = False

getKey :: SDL.Keysym -> Key
getKey keysym = case SDL.keysymKeycode keysym of
                  SDL.SDLK_w     -> W
                  SDL.SDLK_a     -> A
                  SDL.SDLK_s     -> S
                  SDL.SDLK_d     -> D
                  SDL.SDLK_UP    -> UP
                  SDL.SDLK_DOWN  -> DOWN
                  SDL.SDLK_LEFT  -> LEFT
                  SDL.SDLK_RIGHT -> RIGHT
                  _              -> OTHER
