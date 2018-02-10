module Control.Monad where

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
