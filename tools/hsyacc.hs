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

import qualified Control.Monad.Identity  as Identity
import qualified Data.Maybe              as Maybe
import qualified Language.HsYacc         as HsYacc
import qualified Language.HsYacc.Lexing  as Lexing
import qualified Language.HsYacc.Parsing as Parsing

main :: IO ()
main = do
  s <- getContents
  let (tokens, _) = Identity.runIdentity $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s
  let result = Identity.runIdentity $ Parsing.parse Parsing.semanticActions $ Maybe.catMaybes tokens
  case result of
    Nothing ->
      putStrLn "syntax error."
    Just ((defns, ast, codes), _) ->
      let modid = takeModId defns in
      let start = takeStart defns ast in
      let header = takeCodes defns in
      let footer = codes in
      let grm = concatMap (\(left, rights) -> map (\right -> (left, map mapSymbol right)) rights) ast in
        case HsYacc.generateParser modid start header footer grm of
          Nothing ->
            putStrLn "shift/reduce or reduce/reduce conflict."
          Just s' ->
            putStrLn s'
  where
    mapSymbol (Parsing.Terminal t) = HsYacc.T t
    mapSymbol (Parsing.Nonterminal n) = HsYacc.N n

    takeModId [] = "Parsing"
    takeModId (Parsing.DefnModule modid : _) = modid
    takeModId (_ : defns) = takeModId defns

    takeStart [] [] = "start"
    takeStart [] ((start, _) : _) = start
    takeStart (Parsing.DefnStart start : _) _ = start
    takeStart (_ : defns) grm = takeStart defns grm

    takeCodes [] = ""
    takeCodes (Parsing.DefnCodes codes : defns) = codes ++ takeCodes defns
    takeCodes (_ : defns) = takeCodes defns
