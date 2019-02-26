module Ch24_LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

-- read a single character '1', then die
-- equivalent to char '1' >> stop
one' :: Parser b
one' = one >> stop

-- 1. Await a string value
-- 2. Produce a result which may or may not succeed.
--    (A Nothing value means the parse failed.)
-- 3. returns (value produced, remaining string to be parse)
type Parzer a = String -> Maybe (a, String)

-- read two characters, '1', and '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

println :: String -> IO ()
println s = putStrLn ('\n' : s)

test :: IO ()
test = do
  println "stop:"
  testParse stop
  println "one:"
  testParse one
  println "one':"
  testParse one'
  println "oneTwo:"
  testParse oneTwo
  println "oneTwo':"
  testParse oneTwo'
