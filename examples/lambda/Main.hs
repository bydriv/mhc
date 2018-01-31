module Main where

import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State    as State
import qualified Data.Maybe             as Maybe
import qualified Lexing
import qualified Parsing

posOf :: Parsing.Token -> Parsing.Pos
posOf (Parsing.LAMBDA pos) = pos
posOf (Parsing.DOT pos) = pos
posOf (Parsing.LPAREN pos) = pos
posOf (Parsing.RPAREN pos) = pos
posOf (Parsing.ID (pos, _)) = pos

lineOf :: Parsing.Pos -> String -> Int
lineOf pos = succ . length . lines . take pos

main :: IO ()
main = do
  s <- getContents

  let ((tokens0, s'), pos) = flip State.runState 0 $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s
  let tokens = Maybe.catMaybes tokens0

  case s' of
    [] ->
      case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
        Left Nothing ->
          putStrLn $ "line " ++ show (succ (length (lines s))) ++ ": syntax error."
        Left (Just token) ->
          let pos' = posOf token in
          let lnum = lineOf pos' s in
            putStrLn $ "line " ++ show lnum ++ ": syntax error."
        Right (result, _) -> do
          print result
    (c : _) ->
      putStrLn $ "line " ++ show (lineOf pos s) ++ ": unrecognized character: `" ++ [c] ++ "'."
