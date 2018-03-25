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

import qualified Control.Monad.State          as State
import qualified Data.Maybe                   as Maybe
import qualified Language.Haskell2010.Layout  as Layout
import qualified Language.Haskell2010.Lexing  as Lexing
import qualified Language.Haskell2010.Parsing as Parsing
import qualified System.Environment           as Env

lineOf :: Parsing.Pos -> String -> Int
lineOf (pos, len) = length . lines . take (pos + len)

printToken :: Parsing.Token -> IO ()
printToken = putStrLn . Parsing.showToken

main :: IO ()
main = do
  args <- Env.getArgs
  s <- getContents
  let ((tokens0, s'), (pos, _)) = flip State.runState (0, 0) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s

  case s' of
    [] ->
      if "--layout" `elem` args then
        let tokens = Layout.layout (State.evalState (Layout.preprocess tokens0) (0, True)) 0 [] (0, 0) in
          mapM_ printToken tokens
      else
        mapM_ printToken $ Maybe.mapMaybe (\tk -> case tk of {Right tok -> Just tok; Left _ -> Nothing}) tokens0
    (c : _) ->
      putStrLn $ "line " ++ show (lineOf (pos, 1) s) ++ ": unrecognized character: `" ++ [c] ++ "'."
