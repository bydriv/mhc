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

  a = f $ \x -> g $ h x;
  b = let {x = 1 + let {y = 0} in y} in 3 + let {z = x + 1} in z * 2;

  f = (+1);

  s = \x y z -> x z (y z);
  k = \x y -> x;
  i = s k k;

  unary = -1;
  binary = 2 - 3;

  fib :: Int -> Int;
  fib n
    | n == 0, m <- n = 0
    | n == 1, let {m = n} = 1
    | otherwise = fib (n - 2) + fib (n - 1);

  instance Num Int where {
    n + m = n + m
  };

  as :: Int;
  as = 1;

  foreign import ccall safe "static stdio.h puts"
    puts :: Foreign.C.CString -> IO ();

  -- main function
  main :: IO ();
  main = do {
    s <- getContents;
    putStr s
  };
}
