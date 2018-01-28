module Data.JSON where

import qualified Control.Monad.Identity as Identity
import qualified Data.JSON.Lexing       as Lexing
import qualified Data.JSON.Parsing      as Parsing
import qualified Data.Maybe             as Maybe

type JSON = Parsing.JSON

parse :: String -> Maybe JSON
parse s =
  let (tokens0, s') = Identity.runIdentity $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s in
    case s' of
      [] ->
        let tokens = Maybe.catMaybes tokens0 in
          case Identity.runIdentity $ Parsing.parse Parsing.semanticActions tokens of
            Nothing ->
              Nothing
            Just (result, tokens') -> do
              case tokens' of
                [] ->
                  Just result
                _ ->
                  Nothing
      _ ->
        Nothing
