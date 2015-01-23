{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Update where

import FRP.Yampa
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities

import Types
import Config
import Utils

{-
integral' :: SF (Vector3 R) (Vector3 R)
integral' = (iPre zeroVector &&& time) >>> sscan f (zeroVector, 0) >>> arr fst
  where f (!prevVal, !prevTime) (!val, !time')
          | val == zeroVector = ((vectorApply (fromIntegral . round) prevVal), time')
          | otherwise = (prevVal ^+^ (realToFrac $ time' - prevTime) *^ val, time')
          -}

update :: SF ParsedInput (Bool, GameState)
update = proc input@(ParsedInput{ wCount, sCount, sdlQuit }) -> do
  rec speed <- rSwitch selectSpeed -< ((input, pos, speed, obstacles level), winLose `tag` selectSpeed)
      posi <- drSwitch integral -< (speed, winLose `tag` integral)
      pos <- arr calculatePlayerPosition -< (posi, level)
      winLose <- arr testWinLoseCondition -< (pos, level)
      wins <- arr (filterE (== Win)) >>> delayEvent 1 -< winLose
      level <- countHold >>^ fromInteger >>^ (levels !!) -< wins
  returnA -< (sdlQuit, Game { level = level
                            , rotX = realToFrac (wCount - sCount)
                            , playerPos = pos })

  where calculatePlayerPosition (pos, level) = pos ^+^ p3DtoV3 (startingPoint level)
        isOutOfBounds pos sz = let sizeN = fromInteger sz
                               in vector3X pos > sizeN || vector3X pos < 0 ||
                                  vector3Y pos > sizeN || vector3Y pos < 0 ||
                                  vector3Z pos > sizeN || vector3Z pos < 0

        testWinLoseCondition (pos, level)
          | norm (pos ^-^ p3DtoV3 (endPoint level)) < 0.5 = Event Win
          | isOutOfBounds pos (size level)                  = Event Lose
          | otherwise                                       = NoEvent

        countHold = count >>> hold 0

selectSpeed :: SF (ParsedInput, Vector3 R, Vector3 R, [Point3D]) (Vector3 R)
selectSpeed = proc (input, pos, speed, obss) -> do
  let rotX :: Integer
      rotX = fromInteger (floor (wCount input - sCount input) `mod` 36 + 36) `mod` 36
      theta = (((rotX - 6) `div` 9) + 1) `mod` 4
  speedC <- drSwitch (constant zeroVector) -<
    (undefined, tagKeys (upEvents input) speed ((-v) *^ zAxis) theta `merge`
                tagKeys (downEvents input) speed (v *^ zAxis) theta `merge`
                tagKeys (leftEvents input) speed ((-v) *^ xAxis) theta `merge`
                tagKeys (rightEvents input) speed (v *^ xAxis) theta)
  cols <- collision ^>> boolToEvent -< (obss, pos, speedC)
  speedf <- rSwitch (constant zeroVector) -< (speedC, tagCols cols)
  returnA -< speedf

  where xAxis = vector3 1 0 0
        --yAsix = vector3 0 1 0
        zAxis = vector3 0 0 1
        v = 0.5
        collision (obss, pos, speed) = 
          any (\obs -> norm (pos ^+^ ((1/v) *^ speed) ^-^ p3DtoV3 obs) <= 0.4) obss
        tagKeys evCount speed vector theta
          | speed == zeroVector = if evCount == 0 then Event (constant (vector3Rotate' theta vector)) else NoEvent
          | otherwise           = NoEvent
        tagCols cols
          | isNoEvent cols = Event identity
          | otherwise      = cols `tag` constant zeroVector
        boolToEvent = arr (\bool -> if bool then Event () else NoEvent)

