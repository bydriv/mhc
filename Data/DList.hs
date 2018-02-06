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

module Data.DList
  ( DList
  , empty
  , (++)
  , toList
  , fromList ) where

import           Prelude hiding ((++))
import qualified Prelude

newtype DList a = DList ([a] -> [a])

{-# INLINE empty #-}
empty :: DList a
empty = DList id

{-# INLINE (++) #-}
(++) :: DList a -> DList a -> DList a
DList f ++ DList g = DList $ f . g

{-# INLINE toList #-}
toList :: DList a -> [a]
toList (DList f) = f []

{-# INLINE fromList #-}
fromList :: [a] -> DList a
fromList = DList . (Prelude.++)
