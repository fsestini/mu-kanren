{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module DStream where

import Control.Monad

data DStream a = Nil | Cons a (DStream a) | Delay (DStream a)
  deriving (Show, Functor, Foldable, Traversable)

foldS :: (a -> b -> b) -> (b -> b) -> b -> DStream a -> b
foldS f g z Nil = z
foldS f g z (Cons x xs) = f x (foldS f g z xs)
foldS f g z (Delay xs) = g (foldS f g z xs)

instance Applicative DStream where
  pure = flip Cons Nil
  (<*>) = ap

instance Monad DStream where
  return = flip Cons Nil
  xs >>= f = join' $ fmap f xs
    where cat ys = foldS Cons Delay ys ; join' = foldS (flip cat) Delay Nil

interleave :: DStream a -> DStream a -> DStream a
interleave Nil ys = ys
interleave (Delay xs) ys = Delay $ interleave ys xs
interleave (Cons x xs) ys = Cons x (interleave ys xs)

takeLimit :: Int -> Int -> DStream a -> DStream a
takeLimit 0 _ _ = Nil
takeLimit n l Nil = Nil
takeLimit n l (Cons x xs) = Cons x (takeLimit (n - 1) l xs)
takeLimit n 0 (Delay xs) = Nil
takeLimit n l (Delay xs) = Delay (takeLimit n (l - 1) xs)
