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

module Control.Monad.Trans.CodeGenerating
  ( CodeGeneratingT
  , generate
  , write ) where

import qualified Control.Monad             as Monad
import qualified Control.Monad.Trans.State as StateT
import qualified Data.DList                as DList

type CodeGeneratingT s = StateT.StateT (DList.DList s)

{-# INLINE generate #-}
generate :: Monad m => CodeGeneratingT s m () -> m [s]
generate = Monad.liftM DList.toList . flip StateT.execStateT DList.empty

{-# INLINE write #-}
write :: Monad m => [s] -> CodeGeneratingT s m ()
write l = do
  dl <- StateT.get
  StateT.put $ (DList.++) dl $ DList.fromList l
