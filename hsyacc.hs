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

lexingSemanticActions :: Monad m => Lexing.SemanticActions m (Maybe Parsing.Token)
lexingSemanticActions = Lexing.SemanticActions
  { Lexing.saSpace = const $ return Nothing
  , Lexing.saNewline = const $ return Nothing
  , Lexing.saColonEq = const $ return $ Just $ Parsing.COLONEQ ()
  , Lexing.saDef = const $ return $ Just $ Parsing.DEF ()
  , Lexing.saRule = const $ return $ Just $ Parsing.RULE ()
  , Lexing.saPipe = const $ return $ Just $ Parsing.PIPE ()
  , Lexing.saPP = const $ return $ Just $ Parsing.PP ()
  , Lexing.saTerminal = return . Just . Parsing.TERMINAL
  , Lexing.saNonterminal = return . Just . Parsing.NONTERMINAL }

main :: IO ()
main = do
  s <- getContents
  let (tokens, _) = Identity.runIdentity $ Lexing.runLexing $ Lexing.lex lexingSemanticActions s
  let result = Identity.runIdentity $ Parsing.parse Parsing.semanticActions $ Maybe.catMaybes tokens
  case result of
    Nothing ->
      putStrLn "syntax error."
    Just (((), ast, ()), _) ->
      let grm = concatMap (\(left, rights) -> map (\right -> (left, map mapSymbol right)) rights) ast in
        -- TODO: Module Name, start symbol.
        case HsYacc.generateParser "TestParsing" "start" grm of
          Nothing ->
            putStrLn "shift/reduce or reduce/reduce conflict."
          Just s' ->
            putStrLn s'
  where
    mapSymbol (Parsing.Terminal t) = HsYacc.T t
    mapSymbol (Parsing.Nonterminal n) = HsYacc.N n
