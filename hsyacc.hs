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
  { Lexing.initialSpace = const $ return Nothing
  , Lexing.initialNewline = const $ return Nothing
  , Lexing.initialColonEq = const $ return $ Just $ Parsing.COLONEQ ()
  , Lexing.initialDef = const $ return $ Just $ Parsing.DEF ()
  , Lexing.initialRule = const $ return $ Just $ Parsing.RULE ()
  , Lexing.initialPLBrace = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PLBRACE () }
  , Lexing.initialPModule = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PMODULE () }
  , Lexing.initialPP = const $ do { Lexing.yybegin Lexing.Rule; return $ Just $ Parsing.PP () }
  , Lexing.initialPipe = const $ return $ Just $ Parsing.PIPE ()
  , Lexing.initialPWhere = const $ return $ Just $ Parsing.PWHERE ()
  , Lexing.initialPStart = const $ return $ Just $ Parsing.PSTART ()
  , Lexing.initialPRBrace = const $ return $ Just $ Parsing.PRBRACE ()
  , Lexing.initialTerminal = return . Just . Parsing.TERMINAL
  , Lexing.initialNonterminal = return . Just . Parsing.NONTERMINAL
  , Lexing.initialCode = return . Just . Parsing.CODE . head
  , Lexing.ruleSpace = const $ return Nothing
  , Lexing.ruleNewline = const $ return Nothing
  , Lexing.ruleColonEq = const $ return $ Just $ Parsing.COLONEQ ()
  , Lexing.ruleDef = const $ return $ Just $ Parsing.DEF ()
  , Lexing.ruleRule = const $ return $ Just $ Parsing.RULE ()
  , Lexing.rulePLBrace = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PLBRACE () }
  , Lexing.rulePModule = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PMODULE () }
  , Lexing.rulePP = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PP () }
  , Lexing.rulePipe = const $ return $ Just $ Parsing.PIPE ()
  , Lexing.rulePWhere = const $ return $ Just $ Parsing.PWHERE ()
  , Lexing.rulePStart = const $ return $ Just $ Parsing.PSTART ()
  , Lexing.rulePRBrace = const $ return $ Just $ Parsing.PRBRACE ()
  , Lexing.ruleTerminal = return . Just . Parsing.TERMINAL
  , Lexing.ruleNonterminal = return . Just . Parsing.NONTERMINAL
  , Lexing.ruleCode = return . Just . Parsing.CODE . head
  , Lexing.codePLBrace = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PLBRACE () }
  , Lexing.codePModule = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PMODULE () }
  , Lexing.codePP = const $ return $ Just $ Parsing.PP ()
  , Lexing.codePWhere = const $ do { Lexing.yybegin Lexing.Initial; return $ Just $ Parsing.PWHERE () }
  , Lexing.codePStart = const $ return $ Just $ Parsing.PSTART ()
  , Lexing.codePRBrace = const $ do { Lexing.yybegin Lexing.Initial; return $ Just $ Parsing.PRBRACE () }
  , Lexing.codeCode = return . Just . Parsing.CODE . head }

main :: IO ()
main = do
  s <- getContents
  let (tokens, _) = Identity.runIdentity $ Lexing.runLexing $ Lexing.lex lexingSemanticActions s
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
