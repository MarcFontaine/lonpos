----------------------------------------------------------------------------
-- |
-- Module      :  Lonpos.Test
-- Copyright   :  (c) Marc Fontaine 2024
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

module Lonpos.Test
where
import System.FilePath
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Lonpos.BoardToHtml (boardToHtml, boardToSvg)
import Lonpos.SolveSBV

test :: FilePath -> IO ()
test fname = do
  solution <- solvePuzzle
  writeFile (fname <.> "html") $ renderHtml $ boardToHtml solution
  writeFile (fname <.> "svg") $ renderSvg $ boardToSvg solution  
