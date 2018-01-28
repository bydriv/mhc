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

import qualified Control.Monad.Identity as Identity
import qualified Data.Maybe             as Maybe
import qualified Language.HsLex         as HsLex
import qualified Language.HsLex.Lexing  as Lexing
import qualified Language.HsLex.Parsing as Parsing

lexingSemanticActions :: Monad m => Lexing.SemanticActions m (Maybe Parsing.Token)
lexingSemanticActions = Lexing.SemanticActions
  { Lexing.initialSpace = const $ return Nothing
  , Lexing.initialNewline = const $ return Nothing
  , Lexing.initialHat = const $ return $ Just $ Parsing.HAT ()
  , Lexing.initialHyphen = const $ return $ Just $ Parsing.HYPHEN ()
  , Lexing.initialLBracket = const $ return $ Just $ Parsing.LBRACKET ()
  , Lexing.initialRBracket = const $ return $ Just $ Parsing.RBRACKET ()
  , Lexing.initialComma = const $ return $ Just $ Parsing.COMMA ()
  , Lexing.initialDArrow = const $ return $ Just $ Parsing.DARROW ()
  , Lexing.initialCase = const $ return $ Just $ Parsing.CASE ()
  , Lexing.initialStar = const $ return $ Just $ Parsing.STAR ()
  , Lexing.initialPlus = const $ return $ Just $ Parsing.PLUS ()
  , Lexing.initialQues = const $ return $ Just $ Parsing.QUES ()
  , Lexing.initialLParen = const $ return $ Just $ Parsing.LPAREN ()
  , Lexing.initialRParen = const $ return $ Just $ Parsing.RPAREN ()
  , Lexing.initialString = return . Just . Parsing.STRING . init . tail
  , Lexing.initialChar = return . Just . Parsing.CHAR . head . init . tail
  , Lexing.initialHT = const $ return $ Just $ Parsing.CHAR $ '\t'
  , Lexing.initialLF = const $ return $ Just $ Parsing.CHAR $ '\n'
  , Lexing.initialVT = const $ return $ Just $ Parsing.CHAR $ '\v'
  , Lexing.initialFF = const $ return $ Just $ Parsing.CHAR $ '\f'
  , Lexing.initialCR = const $ return $ Just $ Parsing.CHAR $ '\r'
  , Lexing.initialPLBrace = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PLBRACE () }
  , Lexing.initialPModule = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PMODULE () }
  , Lexing.initialPP = const $ do { Lexing.yybegin Lexing.Rule; return $ Just $ Parsing.PP () }
  , Lexing.initialPipe = const $ return $ Just $ Parsing.PIPE ()
  , Lexing.initialPWhere = const $ return $ Just $ Parsing.PWHERE ()
  , Lexing.initialPRBrace = const $ return $ Just $ Parsing.PRBRACE ()
  , Lexing.initialSemanticAction = return . Just . Parsing.SEMANTIC_ACTION
  , Lexing.initialLexingState = return . Just . Parsing.LEXING_STATE
  , Lexing.initialCode = return . Just . Parsing.CODE . head
  , Lexing.ruleSpace = const $ return Nothing
  , Lexing.ruleNewline = const $ return Nothing
  , Lexing.ruleHat = const $ return $ Just $ Parsing.HAT ()
  , Lexing.ruleHyphen = const $ return $ Just $ Parsing.HYPHEN ()
  , Lexing.ruleLBracket = const $ return $ Just $ Parsing.LBRACKET ()
  , Lexing.ruleRBracket = const $ return $ Just $ Parsing.RBRACKET ()
  , Lexing.ruleComma = const $ return $ Just $ Parsing.COMMA ()
  , Lexing.ruleDArrow = const $ return $ Just $ Parsing.DARROW ()
  , Lexing.ruleCase = const $ return $ Just $ Parsing.CASE ()
  , Lexing.ruleStar = const $ return $ Just $ Parsing.STAR ()
  , Lexing.rulePlus = const $ return $ Just $ Parsing.PLUS ()
  , Lexing.ruleQues = const $ return $ Just $ Parsing.QUES ()
  , Lexing.ruleLParen = const $ return $ Just $ Parsing.LPAREN ()
  , Lexing.ruleRParen = const $ return $ Just $ Parsing.RPAREN ()
  , Lexing.ruleString = return . Just . Parsing.STRING . init . tail
  , Lexing.ruleHT = const $ return $ Just $ Parsing.CHAR $ '\t'
  , Lexing.ruleLF = const $ return $ Just $ Parsing.CHAR $ '\n'
  , Lexing.ruleVT = const $ return $ Just $ Parsing.CHAR $ '\v'
  , Lexing.ruleFF = const $ return $ Just $ Parsing.CHAR $ '\f'
  , Lexing.ruleCR = const $ return $ Just $ Parsing.CHAR $ '\r'
  , Lexing.ruleChar = return . Just . Parsing.CHAR . head . init . tail
  , Lexing.rulePLBrace = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PLBRACE () }
  , Lexing.rulePModule = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PMODULE () }
  , Lexing.rulePP = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PP () }
  , Lexing.rulePipe = const $ return $ Just $ Parsing.PIPE ()
  , Lexing.rulePWhere = const $ return $ Just $ Parsing.PWHERE ()
  , Lexing.rulePRBrace = const $ return $ Just $ Parsing.PRBRACE ()
  , Lexing.ruleSemanticAction = return . Just . Parsing.SEMANTIC_ACTION
  , Lexing.ruleLexingState = return . Just . Parsing.LEXING_STATE
  , Lexing.ruleCode = return . Just . Parsing.CODE . head
  , Lexing.codePLBrace = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PLBRACE () }
  , Lexing.codePModule = const $ do { Lexing.yybegin Lexing.Code; return $ Just $ Parsing.PMODULE () }
  , Lexing.codePP = const $ return $ Just $ Parsing.PP ()
  , Lexing.codePWhere = const $ do { Lexing.yybegin Lexing.Initial; return $ Just $ Parsing.PWHERE () }
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
      let header = takeCodes defns in
      let footer = codes in
      putStrLn $ HsLex.generateLexer modid header footer ast
  where
    takeModId [] = "Parsing"
    takeModId (Parsing.DefnModule modid : _) = modid
    takeModId (_ : defns) = takeModId defns

    takeCodes [] = ""
    takeCodes (Parsing.DefnCodes codes : defns) = codes ++ takeCodes defns
    takeCodes (_ : defns) = takeCodes defns
