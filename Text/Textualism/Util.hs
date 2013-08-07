module Text.Textualism.Util where

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right x) = Right x
mapLeft f (Left  x) = Left $ f x
