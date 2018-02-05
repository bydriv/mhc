-- MHC: Minimal Haskell Compiler © 2018 Kaoru Kawamukai <bydriv@gmail.com>
--
-- MHC is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- MHC is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with MHC.  If not, see <http://www.gnu.org/licenses/>.

module Main where

import qualified Control.Monad.Identity as Identity
import qualified Data.Maybe             as Maybe
import qualified Language.HsLex         as HsLex
import qualified Language.HsLex.Lexing  as Lexing
import qualified Language.HsLex.Parsing as Parsing

main :: IO ()
main = do
  s <- getContents
  let (tokens, _) = Identity.runIdentity $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s
  let result = Identity.runIdentity $ Parsing.parse Parsing.semanticActions $ Maybe.catMaybes tokens
  case result of
    Left _ ->
      putStrLn "syntax error."
    Right ((defns, ast, codes), _) ->
      let modid = takeModId defns in
      let header = takeCodes defns in
      let footer = codes in
      putStrLn $ HsLex.generateLexer modid header footer ast
  where
    takeModId [] = "Parsing"
    takeModId (Parsing.DefnModule modid : _) = modid
    takeModId (_ : defns) = takeModId defns

    takeCodes [] = ""
    takeCodes (Parsing.DefnCodes codes : defns) = codes ++ takeCodes defns
    takeCodes (_ : defns) = takeCodes defns
