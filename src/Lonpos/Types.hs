----------------------------------------------------------------------------
-- |
-- Module      :  Lonpos.Types
-- Copyright   :  (c) Marc Fontaine 2024
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Lonpos.Types
where

import Data.Array as Array
import Data.SBV

data Colour
  = Black | Beige | Red | LightBlue | LightGreen | Yellow
  | BlueViolet | DeepPink | Brown | Green | Blue | LightPink
  deriving Enum

mkSymbolicEnumeration ''Colour

-- Black is the empty board.
allPieces :: [ Colour ]
allPieces = [Beige .. LightPink]

type Location = (Int, Int)
type Board = Array.Array Location Colour

boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((0, 0), (9, 9))

testBoard :: Board
testBoard = listArray boardBounds $ repeat Black

allLocations :: [(Int,Int)]
allLocations = Array.indices testBoard

isOnBoard :: (Int, Int) -> Bool
isOnBoard (x, y) =
     (x + y >= 4)
  && (x + y <= 14)
  && (x - y < 5)
  && (y - x < 5)  

boardLoc :: [(Int, Int)]
boardLoc = filter isOnBoard allLocations

borderLoc :: [(Int, Int)]
borderLoc = filter ( not . isOnBoard) allLocations

pieceSize :: Colour -> Int
pieceSize = \case
  Black -> 0
  Beige -> 3
  Red -> 5
  LightBlue -> 5
  LightGreen -> 4
  Yellow -> 5
  BlueViolet -> 4
  DeepPink -> 5
  Brown -> 4
  Green -> 5
  Blue -> 5
  LightPink -> 5

-- Shapes don't need to list the anker point which is at (0,0)
type Shape = [ (Int, Int) ]

-- All possible rotations and flips.
pieceShapes :: Colour -> [ Shape ]
pieceShapes = \case
  Black -> []
  Beige -> allAngles [(1,0), (0,1)]
  Red -> allOrientations [ (0,1), (0,2), (1,1), (1,2)]
  LightBlue -> allAngles [(1,0), (2,0), (0,1), (0,2)]
  LightGreen -> allOrientations [(-1,0), (0,1), (1,0)]
  Yellow -> allAngles [(1,1), (1,0), (-1,1) , (-1,0)]
  BlueViolet -> [ blueViolet, rot90 blueViolet ] 
    where blueViolet = [(1,0), (2,0), (3,0)]
  DeepPink -> allOrientations [(-1,-1),(0,-1),(1,0),(1,1)]
  Brown -> allOrientations [(0,-1), (1,0), (1,1)]
  Green -> allOrientations [(-1,1), (0,1), (1,0), (2,0)]
  Blue -> allOrientations [(1,0), (1,1), (1,2), (1,3)]
  LightPink -> allOrientations [(1,-1), (1,0), (1,1), (1,2)]

rot90 :: Shape -> Shape
rot90 = map (\(dx, dy) -> (-dy, dx))

flipShape :: Shape -> Shape
flipShape  = map (\(dx, dy) -> (-dx, dy))

allAngles :: Shape -> [ Shape ]
allAngles s = [s, rot90 s, rot90 $ rot90 s, rot90 $ rot90 $ rot90 s]

allOrientations :: Shape -> [ Shape ]
allOrientations s = allAngles s ++ (allAngles $ flipShape s)
