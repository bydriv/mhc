module Data.TOML where

import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State    as State
import qualified Data.Maybe             as Maybe
import qualified Data.TOML.Lexing       as Lexing
import qualified Data.TOML.Parsing      as Parsing

type TOML = Parsing.Toml

parse :: String -> Maybe TOML
parse s =
  let (tokens0, s') = flip State.evalState (0, 0) $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s in
    case s' of
      [] ->
        let tokens = Maybe.catMaybes tokens0 in
          case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
            Left _ ->
              Nothing
            Right (result, tokens') -> do
              case tokens' of
                [] ->
                  Just result
                _ ->
                  Nothing
      _ ->
        Nothing
