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
import qualified Language.Haskell2010.Layout  as Layout
import qualified Language.Haskell2010.Lexing  as Lexing
import qualified Language.Haskell2010.Parsing as Parsing
import qualified System.Exit                  as Exit
import qualified System.IO                    as IO

type AST = Parsing.Module'

beginningOfLine :: Parsing.Pos -> String -> Int
beginningOfLine (pos, _) = (pos -) . length . takeWhile (/='\n') . reverse . take pos

endOfLine :: Parsing.Pos -> String -> Int
endOfLine (pos, len) = (+ (pos + len)) . length . takeWhile (/='\n') . drop (pos + len)

lineOf :: Parsing.Pos -> String -> Int
lineOf (pos, len) = length . lines . take (pos + len)

colOf :: Parsing.Pos -> String -> Int
colOf (pos, len) = succ . flip (-) len . length . last . lines . take (pos + len)

substr :: Parsing.Pos -> String -> String
substr (pos, len) = take len . drop pos

highlight :: Parsing.Pos -> String -> String
highlight (pos, len) s =
  let (s1, s') = splitAt pos s in
  let (s2, s3) = splitAt len s' in
    s1 ++ "\027[4m" ++ s2 ++ "\027[0m" ++ s3

main :: IO ()
main = do
  s <- getContents
  let ((tokens0, s'), (pos, _)) = flip State.runState (0, 0) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s

  case s' of
    [] -> do
      let tokens = Layout.layout (State.evalState (Layout.preprocess tokens0) (0, True)) 0 [] (0, 0)

      case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
        Left Nothing ->
          putStrLn $ "line " ++ show (succ (length (lines s))) ++ ": syntax error."
        Left (Just token) ->
          let pos' = Parsing.posOf token in
          let bol = beginningOfLine pos' s in
          let eol = endOfLine pos' s in
          let s'' = substr (bol, eol - bol) s in
          let lineno = lineOf pos' s in
          let colno = colOf pos' s in do
            IO.hPutStrLn IO.stderr $ "\027[1m" ++ show lineno ++ ":" ++ show colno ++ ":\027[0m \027[1;31merror:\027[0m \027[1munexpected token\027[0m"
            IO.hPutStrLn IO.stderr $ highlight (fst pos' - bol, snd pos') s''
            Exit.exitFailure
        Right (result, _) ->
          print result
    (c : _) ->
      putStrLn $ "line " ++ show (lineOf (pos, 1) s) ++ ": unrecognized character: `" ++ [c] ++ "'."
