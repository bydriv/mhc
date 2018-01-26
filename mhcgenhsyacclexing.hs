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
import           Language.HsLex

space :: Regexp
space = generalCategory Char.Space

newline :: Regexp
newline = string "\r\n" ||| string "\r" ||| string "\n" ||| string "\f"

small :: Regexp
small = generalCategory Char.LowercaseLetter

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

rPP :: Regexp
rPP = string "%%"

rRule :: Regexp
rRule = string "rule"

rTerminal :: Regexp
rTerminal = large >>> many (small ||| large ||| digit ||| char '\'')

rNonterminal :: Regexp
rNonterminal = small >>> many (small ||| large ||| digit ||| char '\'')

main :: IO ()
main = putStrLn $ generateLexer "Language.HsYacc.Lexing"
  [ ("Initial", space, "saSpace")
  , ("Initial", newline, "saNewline")
  , ("Initial", rColonEq, "saColonEq")
  , ("Initial", rDef, "saDef")
  , ("Initial", rRule, "saRule")
  , ("Initial", rPipe, "saPipe")
  , ("Initial", rPP, "saPP")
  , ("Initial", rRule, "saRule")
  , ("Initial", rTerminal, "saTerminal")
  , ("Initial", rNonterminal, "saNonterminal") ]
