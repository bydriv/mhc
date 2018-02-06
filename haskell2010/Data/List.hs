{-# LANGUAGE NoImplicitPrelude #-}

module Data.List where {
  (++) :: [a] -> [a] -> [a];
  [] ++ l = l;
  (x : xs) ++ l = x : (xs ++ l);
}
