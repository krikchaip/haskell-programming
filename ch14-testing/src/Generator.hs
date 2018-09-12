module Generator where

import Test.QuickCheck

-- trivial generator of values (Gen is also a Monad)
-- try "sample trivialInt" on Ghci
trivialInt :: Gen Int
trivialInt = return 1

-- create random element generator
-- try "sample' oneThroughThree" on Ghci
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']
