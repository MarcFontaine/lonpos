----------------------------------------------------------------------------
-- |
-- Module      :  Lonpos.BoardToHtml
-- Copyright   :  (c) Marc Fontaine 2024
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lonpos.BoardToHtml
(
  boardToHtml
  , boardToSvg
  , renderBoardToHtml
)
where
import Control.Monad (forM_)
import qualified Data.Array as Array ((!))
import Data.String

import Text.Blaze.Svg  
import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Lonpos.Types

renderBoardToHtml :: Board -> String
renderBoardToHtml = renderHtml . boardToHtml

boardToHtml :: Board -> Html
boardToHtml board
  = docTypeHtml $ do
      H.head $
         H.title "Board"
      body $
          boardToSvg board

boardToSvg :: Board -> Svg
boardToSvg board
  = docTypeSvg $
    svg ! version "1.1"
        ! width "100%"
        ! height "100%"
        ! preserveaspectratio "XMidYMid"
        ! viewbox "0 0 10 10"
    $ do
      forM_ boardLoc $ \pos -> cell pos (board Array.! pos)

cell :: (Int, Int) -> Piece -> Svg
cell (xpos, ypos) piece
  = svg ! version "1.1"
        ! width "1"
        ! height "1"
        ! x (toValue xpos)
        ! y (toValue ypos)
        ! viewbox "-10 -10 20 20"
        ! preserveaspectratio "XMidYMid"
        ! transform "rotate(45 5 5)"
        $ do
          circle
            ! r "10"
            ! (if piece == Board
               then A.style "stroke:Black;stroke-width:0.01" -- TODO
               else fill (fromString $ pieceColor piece)
              )


pieceColor :: Piece -> String
pieceColor = \case
  Board -> "black"
  F -> "beige"
  B -> "red"
  G -> "lightblue"
  S -> "lightgreen"
  I -> "yellow"
  J -> "blueviolet"
  H -> "deepPink"
  R -> "brown"
  E -> "green"
  C -> "blue"
  D -> "lightPink"
