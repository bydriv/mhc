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

import           Language.HsYacc

grm :: Grammar String String
grm =
  [ ("start", [N "definitions", T "PP", N "rules", T "PP", N "codes"])
  , ("definitions", [])
  , ("rules", [])
  , ("rules", [N "rule", N "rules"])
  , ("rule", [T "DEF", T "RULE", N "nonterminal", T "COLONEQ", N "ruleBodies"])
  , ("ruleBodies", [N "ruleBody"])
  , ("ruleBodies", [N "ruleBody", T "PIPE", N "ruleBodies"])
  , ("ruleBody", [])
  , ("ruleBody", [N "symbol", N "ruleBody"])
  , ("symbol", [N "terminal"])
  , ("symbol", [N "nonterminal"])
  , ("terminal", [T "TERMINAL"])
  , ("nonterminal", [T "NONTERMINAL"])
  , ("codes", []) ]

main :: IO ()
main =
  case generateParser "Language.HsYacc.Parsing" "start" "" "" grm of
    Nothing ->
      putStrLn "shift/reduce or reduce/reduce conflict."
    Just s ->
      putStrLn s
