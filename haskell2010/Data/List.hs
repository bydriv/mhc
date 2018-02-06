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

module Data.List where {
  (++) :: [a] -> [a] -> [a];
  [] ++ l = l;
  (x : xs) ++ l = x : (xs ++ l);

  head :: [a] -> a;
  head (x : _) = x;

  last :: [a] -> a;
  last [x] = x;
  last (_ : xs) = last xs;

  tail :: [a] -> [a];
  tail (_ : xs) = xs;

  init :: [a] -> [a];
  init [x] = [];
  init (x : xs) = x : init xs;

  null :: [a] -> Bool;
  null [] = True;
  null (_ : _) = False;

  length :: [a] -> Int;
  length [] = 0;
  length (_ : xs) = length xs + 1;
}
