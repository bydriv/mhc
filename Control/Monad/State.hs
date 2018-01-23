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

module Control.Monad.State
  ( State
  , runState
  , evalState
  , execState
  , get
  , put
  , gets
  , modify ) where

import qualified Control.Monad.Identity    as Identity
import qualified Control.Monad.Trans.State as StateT

type State s = StateT.StateT s Identity.Identity

runState :: State s a -> s -> (a, s)
runState m = Identity.runIdentity . StateT.runStateT m

evalState :: State s a -> s -> a
evalState m = fst . Identity.runIdentity . StateT.runStateT m

execState :: State s a -> s -> s
execState m = snd . Identity.runIdentity . StateT.runStateT m

get :: State s s
get = StateT.get

put :: s -> State s ()
put = StateT.put

gets :: (s -> a) -> State s a
gets = StateT.gets

modify :: (s -> s) -> State s ()
modify = StateT.modify
