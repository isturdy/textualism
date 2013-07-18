module Text.Textualism.Util where

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

foldUntil' :: (a -> b -> Either a a) -> a -> [b] -> a
foldUntil' _ a []     = a
foldUntil' f a (x:xs) = case f a x of
  Right v -> v
  Left a' -> a' `seq` foldUntil' f a' xs
