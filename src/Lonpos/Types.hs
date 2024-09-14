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

-- This only covers the lonpos200+ pieces.
-- There are other lonpos puzzles with additional pieces

data Piece
  = Board | B | C | D | E | F | G | H | I | J | R | S
  deriving Enum

mkSymbolicEnumeration ''Piece

-- Board is the empty board.
-- lonpos200+
allPieces :: [ Piece ]
allPieces = [ B, C, D, E, F, G, H, I, J, R, S ]

type Location = (Int, Int)
type Board = Array.Array Location Piece

boardBounds :: ((Int, Int), (Int, Int))
boardBounds = ((0, 0), (9, 9))

emptyBoard :: Board
emptyBoard = listArray boardBounds $ repeat Board

allLocations :: [(Int,Int)]
allLocations = Array.indices emptyBoard

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

pieceSize :: Piece -> Int
pieceSize c = (length $ head $ pieceShapes c) + 1

-- Shapes don't need to list the anker point which is at (0,0)
type Shape = [ (Int, Int) ]

-- All possible rotations and flips.
pieceShapes :: Piece -> [ Shape ]
pieceShapes = \case
  Board -> []
  F -> allAngles [(1,0), (0,1)]
  B -> allOrientations [ (0,1), (0,2), (1,1), (1,2)]
  G -> allAngles [(1,0), (2,0), (0,1), (0,2)]
  S -> allAngles [(-1,0), (0,1), (1,0)]
  I -> allAngles [(1,1), (1,0), (-1,1) , (-1,0)]
  J -> [ j, rot90 j ]
    where j = [(1,0), (2,0), (3,0)]
  H -> allAngles [(-1,-1), (0,-1), (1,0), (1,1)]
  R -> [ r, rot90 r, flipShape r, rot90 $ flipShape r ]
    where r = [(0,-1), (1,0), (1,1)]
  E -> allOrientations [(-1,1), (0,1), (1,0), (2,0)]
  C -> allOrientations [(1,0), (1,1), (1,2), (1,3)]
  D -> allOrientations [(1,-1), (1,0), (1,1), (1,2)]

rot90 :: Shape -> Shape
rot90 = map (\(dx, dy) -> (-dy, dx))

flipShape :: Shape -> Shape
flipShape  = map (\(dx, dy) -> (-dx, dy))

allAngles :: Shape -> [ Shape ]
allAngles s = [s, rot90 s, rot90 $ rot90 s, rot90 $ rot90 $ rot90 s]

allOrientations :: Shape -> [ Shape ]
allOrientations s = allAngles s ++ (allAngles $ flipShape s)
