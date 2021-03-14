module Utils where

splitOn :: (a -> Bool) -> [a] -> [(Maybe a, [a])]
splitOn p [] = []
splitOn p (a:as)
  | p a       = (Just a, takeWhile (not . p) as) : splitOn p (dropWhile (not . p) as)
  | otherwise = (Nothing, a : takeWhile (not . p) as) : splitOn p (dropWhile (not . p) as)
