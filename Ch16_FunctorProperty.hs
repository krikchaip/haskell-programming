module Ch16_FunctorProperty where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == id f

functorCompose :: (Eq (f c), Functor f)
               => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

main :: IO ()
main = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose :: Fun Int Char -> Fun Char Bool -> [Int] -> Bool)
