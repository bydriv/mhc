module Data.Scheme where

import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State    as State
import qualified Data.Maybe             as Maybe
import qualified Data.Scheme.Lexing     as Lexing
import qualified Data.Scheme.Parsing    as Parsing

type Datum = Parsing.Datum

parse :: String -> Maybe Datum
parse s =
  let (tokens0, s') = flip State.evalState 0 $ Lexing.runLexing $ Lexing.lex Lexing.semanticActions s in
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
