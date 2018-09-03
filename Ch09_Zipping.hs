module Ch09_Zipping where

zip' :: [a] -> [b] -> [(a, b)]
zip' []     []     = []
zip' _      []     = []
zip' []     _      = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

zipWith' :: (a  ->  b  ->  c)
         -> [a] -> [b] -> [c]
zipWith' _ []     []     = []
zipWith' _ _      []     = []
zipWith' _ []     _      = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (\x y -> (x, y))
