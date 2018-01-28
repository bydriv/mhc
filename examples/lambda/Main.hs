module Main where

import qualified Control.Monad.Identity as Identity
import qualified Data.Maybe             as Maybe
import qualified Lexing
import qualified Parsing

main :: IO ()
main = do
  s <- getContents

  let (tokens0, s') = Identity.runIdentity $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s
  let tokens = Maybe.catMaybes tokens0

  putStr "tokens: "
  print tokens
  putStr "unread buffer: "
  print s'

  case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
    Nothing ->
      putStrLn "syntax error."
    Just (result, tokens') -> do
      putStr "result: "
      print result
      putStr "unread tokens: "
      print tokens'
