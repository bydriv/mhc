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

module Control.Monad.Trans.State
  ( StateT
  , runStateT
  , evalStateT
  , execStateT
  , get
  , put
  , gets
  , modify ) where

import qualified Control.Applicative as Applicative
import qualified Control.Monad       as Monad
import qualified Control.Monad.Trans as MonadTrans

newtype StateT s m a = StateT (s -> m (a, s))

instance Monad m => Functor (StateT s m) where
  fmap = Monad.liftM

instance Monad m => Applicative.Applicative (StateT s m) where
  pure = return
  (<*>) = Monad.ap

instance Monad m => Monad (StateT s m) where
  return x = StateT $ \s -> return (x, s)

  m >>= k = StateT $ \s -> do
    (x, s') <- runStateT m s
    runStateT (k x) s'

instance MonadTrans.MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    x <- m
    return (x, s)

{-# INLINE runStateT #-}
runStateT :: StateT s m a -> s -> m (a, s)
runStateT (StateT f) = f

{-# INLINE evalStateT #-}
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m = Monad.liftM fst . runStateT m

{-# INLINE execStateT #-}
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m = Monad.liftM snd . runStateT m

{-# INLINE get #-}
get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

{-# INLINE put #-}
put :: Monad m => s -> StateT s m ()
put s = StateT $ const $ return ((), s)

{-# INLINE gets #-}
gets :: Monad m => (s -> a) -> StateT s m a
gets = flip fmap get

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> StateT s m ()
modify f = get >>= put . f
