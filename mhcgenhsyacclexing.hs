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

import qualified Data.Char      as Char
import           Language.HsLex as HsLex

space :: Regexp
space = generalCategory Char.Space

newline :: Regexp
newline = string "\r\n" ||| string "\r" ||| string "\n" ||| string "\f"

small :: Regexp
small = generalCategory Char.LowercaseLetter ||| char '_'

large :: Regexp
large = generalCategory Char.UppercaseLetter ||| generalCategory Char.TitlecaseLetter

digit :: Regexp
digit = generalCategory Char.DecimalNumber

rColonEq :: Regexp
rColonEq = string ":="

rDef :: Regexp
rDef = string "def"

rPipe :: Regexp
rPipe = string "|"

rPLBrace :: Regexp
rPLBrace = string "%{"

rPModule :: Regexp
rPModule = string "%module"

rPP :: Regexp
rPP = string "%%"

rPStart :: Regexp
rPStart = string "%start"

rPWhere :: Regexp
rPWhere = string "%where"

rPRBrace :: Regexp
rPRBrace = string "%}"

rRule :: Regexp
rRule = string "rule"

rTerminal :: Regexp
rTerminal = large >>> many (small ||| large ||| digit ||| char '\'')

rNonterminal :: Regexp
rNonterminal = small >>> many (small ||| large ||| digit ||| char '\'')

main :: IO ()
main = putStrLn $ generateLexer "Language.HsYacc.Lexing" "" ""
  [ ("Initial", space, "initialSpace")
  , ("Initial", newline, "initialNewline")
  , ("Initial", rColonEq, "initialColonEq")
  , ("Initial", rDef, "initialDef")
  , ("Initial", rPLBrace, "initialPLBrace")
  , ("Initial", rPModule, "initialPModule")
  , ("Initial", rPP, "initialPP")
  , ("Initial", rPipe, "initialPipe")
  , ("Initial", rPWhere, "initialPWhere")
  , ("Initial", rPStart, "initialPStart")
  , ("Initial", rPRBrace, "initialPRBrace")
  , ("Initial", rRule, "initialRule")
  , ("Initial", rTerminal, "initialTerminal")
  , ("Initial", rNonterminal, "initialNonterminal")
  , ("Initial", HsLex.any, "initialCode")
  , ("Rule", space, "ruleSpace")
  , ("Rule", newline, "ruleNewline")
  , ("Rule", rColonEq, "ruleColonEq")
  , ("Rule", rDef, "ruleDef")
  , ("Rule", rPLBrace, "rulePLBrace")
  , ("Rule", rPModule, "rulePModule")
  , ("Rule", rPP, "rulePP")
  , ("Rule", rPipe, "rulePipe")
  , ("Rule", rPWhere, "rulePWhere")
  , ("Rule", rPStart, "rulePStart")
  , ("Rule", rPRBrace, "rulePRBrace")
  , ("Rule", rRule, "ruleRule")
  , ("Rule", rTerminal, "ruleTerminal")
  , ("Rule", rNonterminal, "ruleNonterminal")
  , ("Rule", HsLex.any, "ruleCode")
  , ("Code", rPLBrace, "codePLBrace")
  , ("Code", rPModule, "codePModule")
  , ("Code", rPP, "codePP")
  , ("Code", rPWhere, "codePWhere")
  , ("Code", rPStart, "codePStart")
  , ("Code", rPRBrace, "codePRBrace")
  , ("Code", HsLex.any, "codeCode") ]
