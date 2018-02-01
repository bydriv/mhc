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

import qualified Control.Monad.Identity       as Identity
import qualified Control.Monad.State          as State
import qualified Data.Maybe                   as Maybe
import qualified Language.Haskell2010.Lexing  as Lexing
import qualified Language.Haskell2010.Parsing as Parsing

type AST = Parsing.Module'

posOf :: Parsing.Token -> Parsing.Pos
posOf (Parsing.ARROW pos) = pos
posOf (Parsing.AS pos) = pos
posOf (Parsing.BACKQUOTE pos) = pos
posOf (Parsing.CLASS pos) = pos
posOf (Parsing.COLON_COLON pos) = pos
posOf (Parsing.COMMA pos) = pos
posOf (Parsing.DARROW pos) = pos
posOf (Parsing.DATA pos) = pos
posOf (Parsing.DEFAULT pos) = pos
posOf (Parsing.DERIVING pos) = pos
posOf (Parsing.DOT_DOT pos) = pos
posOf (Parsing.EQUAL pos) = pos
posOf (Parsing.EXCL pos) = pos
posOf (Parsing.FOREIGN pos) = pos
posOf (Parsing.HIDING pos) = pos
posOf (Parsing.IMPORT pos) = pos
posOf (Parsing.INFIX pos) = pos
posOf (Parsing.INFIXL pos) = pos
posOf (Parsing.INFIXR pos) = pos
posOf (Parsing.INSTANCE pos) = pos
posOf (Parsing.INTEGER (pos, _)) = pos
posOf (Parsing.LBRACE pos) = pos
posOf (Parsing.LBRACKET pos) = pos
posOf (Parsing.LPAREN pos) = pos
posOf (Parsing.MODULE pos) = pos
posOf (Parsing.NEWTYPE pos) = pos
posOf (Parsing.PIPE pos) = pos
posOf (Parsing.QCONID (pos, _)) = pos
posOf (Parsing.QCONSYM (pos, _)) = pos
posOf (Parsing.QUALIFIED pos) = pos
posOf (Parsing.QVARID (pos, _)) = pos
posOf (Parsing.QVARSYM (pos, _)) = pos
posOf (Parsing.RBRACE pos) = pos
posOf (Parsing.RBRACKET pos) = pos
posOf (Parsing.RPAREN pos) = pos
posOf (Parsing.SEMICOLON pos) = pos
posOf (Parsing.TYPE pos) = pos
posOf (Parsing.WHERE pos) = pos

lineOf :: Parsing.Pos -> String -> Int
lineOf (pos, len) = length . lines . take (pos + len)

main :: IO ()
main = do
  s <- getContents
  let ((tokens0, s'), (pos, _)) = flip State.runState (0, 0) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s

  case s' of
    [] -> do
      let tokens = Maybe.catMaybes tokens0

      case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
        Left Nothing ->
          putStrLn $ "line " ++ show (succ (length (lines s))) ++ ": syntax error."
        Left (Just token) ->
          let pos' = posOf token in
          let lnum = lineOf pos' s in
            putStrLn $ "line " ++ show lnum ++ ": syntax error."
        Right (result, _) ->
          print result
    (c : _) ->
      putStrLn $ "line " ++ show (lineOf (pos, 1) s) ++ ": unrecognized character: `" ++ [c] ++ "'."
