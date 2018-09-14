{-# LANGUAGE DeriveGeneric #-}

module CoArbitrary where

import GHC.Generics
import Test.QuickCheck

{-
  more info:
  https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html
-}

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary
