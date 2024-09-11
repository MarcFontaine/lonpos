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
import Data.SBV 
import qualified Data.Array as Array
import Data.Array (Array, (!))

import Lonpos.Types

solvePuzzle :: IO Board
solvePuzzle = do
  let sBoard = makeBoard
  result <- satWith z3 {verbose = False, timing = PrintTiming} sBoard
  case getModelAssignment result of
    Left err -> do
      print err 
      error "no result"
    Right (False, res) -> return $ parseResult res
    _ -> error "probable result ?"

parseResult :: ([Colour], [Bool]) -> Board
parseResult (colours, _ankerPoints)
  = Array.listArray boardBounds colours

type SBoard = Array (Int, Int) SColour

declareSBoard :: Symbolic SBoard
declareSBoard = do
  let names = [ "Colour" ++ show (x,y) | x :: Int <- [0..9] , y :: Int <- [0..9] ]
  vars <- mapM free names
  return $ Array.listArray boardBounds vars

declareAnkerPoints :: Symbolic (Array (Int, Int) SBool)
declareAnkerPoints = do
  let names = [ "PieceAnker" ++ show (x,y) | x :: Int <- [0..9] , y :: Int <- [0..9] ]
  vars <- mapM free names
  return $ Array.listArray boardBounds vars

makeBoard :: Symbolic ()
makeBoard = do
  sBoard <- declareSBoard
  sAnkers <- declareAnkerPoints
  let
    forAll s p = sAnd $ map p s
    forAllPieces = forAll allPieces
    
    blackBorder = forAll borderLoc $ \loc -> (sBoard ! loc) .== sBlack
    allColourCounts = forAllPieces (\c -> colourCount c $ pieceSize c)
    colourCount c n = pbExactly (map (isAt sBoard c) boardLoc ) n
          
    allPiecesAreAnkered = forAllPieces isAnkeredPiece
    isAnkeredPiece c = pbExactly (map (isAnkerPoint c) boardLoc ) 1
    isAnkerPoint c loc = isAt sBoard c loc .&& (sAnkers ! loc)

    isPieceAnker p loc
      = isAnkerPoint p loc .=> sOr (map (matchShape loc p) $ pieceShapes p)

    matchShape (x0,y0) colour shape
      = forAll shape $ \(dx,dy) -> isAt sBoard colour (x0 + dx, y0 + dy)
  
    allPiecesShape = forAll boardLoc $ \loc -> (
      forAll
        [ BlueViolet
        , Beige
        , Yellow
        , LightBlue
        , LightGreen
        , LightPink
        , Blue
        , Brown
        , Red
        , Green
        , DeepPink
        ]
        $ \p -> isPieceAnker p loc
      )

  constrain allColourCounts
  constrain blackBorder
  constrain allPiecesAreAnkered
  constrain allPiecesShape
  constrain $ challenge100 sBoard
  return ()

isAt :: SBoard -> Colour -> (Int, Int) -> SBool
isAt board c loc = if isOnBoard loc
  then (board ! loc) .== literal c
  else sFalse


-- (0,4) is upper left corner
-- (1,3) is right of (0,4)

-- result: 5.5s
challenge100 :: SBoard -> SBool
challenge100 b = sAnd [
    isAt b Beige (0,4)
  , isAt b Beige (1,3)
  , isAt b Beige (1,4)
  , isAt b Green (2,2)
  , isAt b Green (3,1)
  , isAt b Green (2,3)
  , isAt b Green (3,2)
  , isAt b Green (2,4)
  ]

-- result: 17s
challenge99 :: SBoard -> SBool
challenge99 b = sAnd [
    isAt b Red (0,4)
  , isAt b Red (1,4)
  , isAt b Red (2,4)
  , isAt b DeepPink (2,2)
  , isAt b DeepPink (3,1)
  , isAt b DeepPink (2,3)
  , isAt b DeepPink (3,2)
  , isAt b DeepPink (1,3)
  ]
