module Exercises.Gen where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

oddEqual :: Gen Fool
oddEqual = elements [Fulse, Frue]

inThird :: Gen Fool
inThird = frequency [(2, return Fulse)
                    ,(1, return Frue)]
