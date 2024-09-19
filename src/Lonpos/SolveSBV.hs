----------------------------------------------------------------------------
-- |
-- Module      :  Lonpos.SolveSBV
-- Copyright   :  (c) Marc Fontaine 2024
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

module Lonpos.SolveSBV
where
import           Data.Array   (Array, (!))
import qualified Data.Array   as Array
import           Data.SBV

import           Lonpos.Types

solvePuzzle :: IO Board
solvePuzzle = do
  let board = makeBoard
  result <- satWith z3 {verbose = False, timing = PrintTiming} board
  case getModelAssignment result of
    Left err -> do
      print err
      error "no result"
    Right (False, res) -> return $ parseResult res
    _ -> error "probable result ?"

parseResult :: ([Piece], [Bool]) -> Board
parseResult (colours, _ankerPoints)
  = Array.listArray boardBounds colours

type SBoard = Array (Int, Int) SPiece

declareSBoard :: Symbolic SBoard
declareSBoard = do
  let names = [ "Piece" ++ show (x,y) | x :: Int <- [0..9] , y :: Int <- [0..9] ]
  vars <- mapM free names
  return $ Array.listArray boardBounds vars

declareAnkerPoints :: Symbolic (Array (Int, Int) SBool)
declareAnkerPoints = do
  let names = [ "PieceAnker" ++ show (x,y) | x :: Int <- [0..9] , y :: Int <- [0..9] ]
  vars <- mapM free names
  return $ Array.listArray boardBounds vars

makeBoard :: Symbolic ()
makeBoard = do
  board <- declareSBoard
  sAnkers <- declareAnkerPoints
  let
    forAll s p = sAnd $ map p s
    forAllPieces = forAll allPieces

    blackBorder = forAll borderLoc $ \loc -> (board ! loc) .== literal Board
    borderNotAnker = forAll borderLoc $ \loc -> (sAnkers ! loc) .== sFalse
    allPieceCounts = forAllPieces (\c -> colourCount c $ pieceSize c)
    colourCount c n = pbExactly (map (isAt board c) boardLoc ) n

    boardNotBlack = forAll boardLoc $ \loc -> sNot ((board ! loc) .== literal Board)

    allPiecesAreAnkered = forAllPieces isAnkeredPiece
    isAnkeredPiece c = pbExactly (map (isAnkerPoint c) boardLoc ) 1
    isAnkerPoint c loc = isAt board c loc .&& (sAnkers ! loc)

    isPieceAnker p loc
      = isAnkerPoint p loc .=> sOr (map (matchShape loc p) $ pieceShapes p)

    matchShape (x0,y0) colour shape
      = forAll shape $ \(dx,dy) -> isAt board colour (x0 + dx, y0 + dy)

    allPiecesShape = forAll boardLoc $ \loc ->
      forAllPieces $ \p -> isPieceAnker p loc

  constrain allPieceCounts
  constrain blackBorder
  constrain borderNotAnker
  constrain boardNotBlack
  constrain allPiecesAreAnkered
  constrain allPiecesShape
  constrain $ challenge95 board
  return ()

isAt :: SBoard -> Piece -> (Int, Int) -> SBool
isAt board c loc = if isOnBoard loc
  then (board ! loc) .== literal c
  else sFalse

-- First result:
-- *** SBV: Elapsed time: 1.388s
-- Total : 45 solutions in 4 minutes
challenge100 :: SBoard -> SBool
challenge100 b = sAnd [
    isAt b F (0,4)
  , isAt b F (1,3)
  , isAt b F (1,4)
  , isAt b E (2,2)
  , isAt b E (3,1)
  , isAt b E (2,3)
  , isAt b E (3,2)
  , isAt b E (2,4)
  ]

-- Result: 17s
challenge99 :: SBoard -> SBool
challenge99 b = sAnd [
    isAt b B (0,4)
  , isAt b B (1,4)
  , isAt b B (2,4)
  , isAt b H (2,2)
  , isAt b H (3,1)
  , isAt b H (2,3)
  , isAt b H (3,2)
  , isAt b H (1,3)
  ]

-- Result:
-- *** SBV: Elapsed time: 54.157s

challenge98 :: SBoard -> SBool
challenge98 b = sAnd [
    isAt b B (0,4)
  , isAt b B (1,3)
  , isAt b B (1,4)
  , isAt b B (2,3)
  , isAt b S (1,5)
  , isAt b S (2,5)
  , isAt b S (3,5)
  , isAt b S (2,6)
  ]

-- Result:
-- *** SBV: Elapsed time: 1.090s
challenge97 :: SBoard -> SBool
challenge97 b = sAnd [
    isAt b R (0,4)
  , isAt b R (1,3)
  , isAt b R (1,4)
  , isAt b R (2,3)
  , isAt b C (1,5)
  , isAt b C (2,5)
  , isAt b C (3,5)
  , isAt b C (4,5)
  , isAt b C (4,4)
  , isAt b H (2,6)
  , isAt b H (3,6)
  , isAt b H (3,7)
  , isAt b H (4,8)
  ]

-- Result:
-- *** SBV: Elapsed time: 2.514s
challenge96 :: SBoard -> SBool
challenge96 b = sAnd [
    isAt b S (0,4)
  , isAt b S (1,3)
  , isAt b S (1,4)
  , isAt b S (1,5)
  , isAt b E (2,2)
  , isAt b E (3,1)
  , isAt b E (2,3)
  , isAt b E (3,2)
  , isAt b E (2,4)
  , isAt b B (2,5)
  , isAt b B (2,6)
  , isAt b B (3,5)
  , isAt b B (3,6)
  , isAt b B (3,7)
  ]

-- Result:
-- *** SBV: Elapsed time: 4.132s
challenge95 :: SBoard -> SBool
challenge95 b = sAnd [
    isAt b B (0,4)
  , isAt b B (1,4)
  , isAt b B (1,5)
  , isAt b B (2,4)
  , isAt b B (2,5)
  , isAt b H (1,3)
  , isAt b H (2,3)
  , isAt b H (2,2)
  , isAt b H (3,2)
  , isAt b H (3,1)
  , isAt b F (4,0)
  , isAt b F (4,1)
  , isAt b F (5,1)
  ]
