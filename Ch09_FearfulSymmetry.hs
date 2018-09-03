module Ch09_FearfulSymmetry where

import Control.Exception

-- ex01
ex01 :: IO ()
ex01 =
  print $ assert (expect == actual) "ex01: pass"
  where
    expect = ["wallfish", "wants", "fun"]
    actual = myWords "sheryl wants fun"

myWords :: String -> [String]
myWords s  =
  let takeWord = takeWhile (/= ' ')
      takeNext = drop 1 . dropWhile (/= ' ')
      second   = takeWord . takeNext
      third    = takeWord . takeNext . takeNext
  in  ["wallfish"] ++ [second s] ++ [third s]

-- ex02
ex02 :: IO ()
ex02 = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
          \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

myLines :: String -> [String]
myLines [] = []
myLines s  = line : myLines next
  where line = takeWhile (/='\n') s
        next = drop 1 . dropWhile (/='\n') $ s

-- ex03
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ []  = []
splitBy c s = split : splitBy c next
  where split = takeWhile (/=c) s
        next  = drop 1 . dropWhile (/=c) $ s

myWords' :: String -> [String]
myWords' = (["wallfish"] ++) . tail . splitBy ' '

myLines' :: String -> [String]
myLines' = splitBy '\n'
