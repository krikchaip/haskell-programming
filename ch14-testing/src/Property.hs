module Property where

import Test.QuickCheck

{- Using QuickCheck without Hspec -}

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionGreater' :: Int -> Bool
prop_additionGreater' x = x + 0 > x

runQc :: IO ()
runQc = do
  quickCheck prop_additionGreater  -- success case
  quickCheck prop_additionGreater' -- failure case
