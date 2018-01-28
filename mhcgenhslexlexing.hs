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

rHAT :: Regexp
rHAT = string "^"

rHYPHEN :: Regexp
rHYPHEN = string "-"

rLBRACKET :: Regexp
rLBRACKET = string "["

rRBRACKET :: Regexp
rRBRACKET = string "]"

rComma :: Regexp
rComma = string ","

rDArrow :: Regexp
rDArrow = string "=>"

rCase :: Regexp
rCase = string "case"

rStar :: Regexp
rStar = string "*"

rPlus :: Regexp
rPlus = string "+"

rQues :: Regexp
rQues = string "?"

rLParen :: Regexp
rLParen = string "("

rRParen :: Regexp
rRParen = string ")"

rString :: Regexp
rString = char '"' >>> many (noneOf ['"'] ||| string "\\\"") >>> char '"'

rChar :: Regexp
rChar = char '\'' >>> HsLex.any >>> char '\''

rPipe :: Regexp
rPipe = string "|"

rPLBrace :: Regexp
rPLBrace = string "%{"

rPModule :: Regexp
rPModule = string "%module"

rPP :: Regexp
rPP = string "%%"

rPWhere :: Regexp
rPWhere = string "%where"

rPRBrace :: Regexp
rPRBrace = string "%}"

rSemanticAction :: Regexp
rSemanticAction = small >>> many (small ||| large ||| digit ||| char '\'')

rLexingState :: Regexp
rLexingState = large >>> many (small ||| large ||| digit ||| char '\'')

main :: IO ()
main = putStrLn $ generateLexer "Language.HsLex.Lexing" "" ""
  [ ("Initial", space, "initialSpace")
  , ("Initial", newline, "initialNewline")
  , ("Initial", rHAT, "initialHat")
  , ("Initial", rHYPHEN, "initialHyphen")
  , ("Initial", rLBRACKET, "initialLBracket")
  , ("Initial", rRBRACKET, "initialRBracket")
  , ("Initial", rComma, "initialComma")
  , ("Initial", rDArrow, "initialDArrow")
  , ("Initial", rCase, "initialCase")
  , ("Initial", rStar, "initialStar")
  , ("Initial", rPlus, "initialPlus")
  , ("Initial", rQues, "initialQues")
  , ("Initial", rLParen, "initialLParen")
  , ("Initial", rRParen, "initialRParen")
  , ("Initial", rString, "initialString")
  , ("Initial", string "'\\t'", "initialHT")
  , ("Initial", string "'\\n'", "initialLF")
  , ("Initial", string "'\\v'", "initialVT")
  , ("Initial", string "'\\f'", "initialFF")
  , ("Initial", string "'\\r'", "initialCR")
  , ("Initial", rChar, "initialChar")
  , ("Initial", rPLBrace, "initialPLBrace")
  , ("Initial", rPModule, "initialPModule")
  , ("Initial", rPP, "initialPP")
  , ("Initial", rPipe, "initialPipe")
  , ("Initial", rPWhere, "initialPWhere")
  , ("Initial", rPRBrace, "initialPRBrace")
  , ("Initial", rSemanticAction, "initialSemanticAction")
  , ("Initial", rLexingState, "initialLexingState")
  , ("Initial", HsLex.any, "initialCode")
  , ("Rule", space, "ruleSpace")
  , ("Rule", newline, "ruleNewline")
  , ("Rule", rHAT, "ruleHat")
  , ("Rule", rHYPHEN, "ruleHyphen")
  , ("Rule", rLBRACKET, "ruleLBracket")
  , ("Rule", rRBRACKET, "ruleRBracket")
  , ("Rule", rComma, "ruleComma")
  , ("Rule", rDArrow, "ruleDArrow")
  , ("Rule", rCase, "ruleCase")
  , ("Rule", rStar, "ruleStar")
  , ("Rule", rPlus, "rulePlus")
  , ("Rule", rQues, "ruleQues")
  , ("Rule", rLParen, "ruleLParen")
  , ("Rule", rRParen, "ruleRParen")
  , ("Rule", rString, "ruleString")
  , ("Rule", string "'\\t'", "ruleHT")
  , ("Rule", string "'\\n'", "ruleLF")
  , ("Rule", string "'\\v'", "ruleVT")
  , ("Rule", string "'\\f'", "ruleFF")
  , ("Rule", string "'\\r'", "ruleCR")
  , ("Rule", rChar, "ruleChar")
  , ("Rule", rPLBrace, "rulePLBrace")
  , ("Rule", rPModule, "rulePModule")
  , ("Rule", rPP, "rulePP")
  , ("Rule", rPipe, "rulePipe")
  , ("Rule", rPWhere, "rulePWhere")
  , ("Rule", rPRBrace, "rulePRBrace")
  , ("Rule", rSemanticAction, "ruleSemanticAction")
  , ("Rule", rLexingState, "ruleLexingState")
  , ("Rule", HsLex.any, "ruleCode")
  , ("Code", rPLBrace, "codePLBrace")
  , ("Code", rPModule, "codePModule")
  , ("Code", rPP, "codePP")
  , ("Code", rPWhere, "codePWhere")
  , ("Code", rPRBrace, "codePRBrace")
  , ("Code", HsLex.any, "codeCode") ]
