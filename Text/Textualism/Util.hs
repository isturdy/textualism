module Text.Textualism.Util where

foldUntil' :: (a -> b -> Either a a) -> a -> [b] -> a
foldUntil' _ a []     = a
foldUntil' f a (x:xs) = case f a x of
  Right v -> v
  Left a' -> a' `seq` foldUntil' f a' xs
