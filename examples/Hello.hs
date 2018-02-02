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

{-
  comment
  {-
    is
  -}
  nestable
-}

module Main where {
  import qualified Control.Monad as Monad;

  as :: Int;
  as = 1;

  foreign import ccall safe "static stdio.h puts"
    puts :: Foreign.C.CString -> IO ();

  -- main function
  main :: IO ();
  main = print 42;
}