-- MHC: Minimal Haskell Compiler © 2017 Kaoru Kawamukai <bydriv@gmail.com>
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

module Control.Monad.Identity (Identity, runIdentity) where

import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad

newtype Identity a = Identity a

instance Functor Identity where
  fmap = Monad.liftM

instance Applicative.Applicative Identity where
  pure = return
  (<*>) = Monad.ap

instance Monad Identity where
  return = Identity
  m >>= k = k $ runIdentity m

runIdentity :: Identity a -> a
runIdentity (Identity x) = x
