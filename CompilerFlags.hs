-- If you had not included the NoMonomorphismRestriction extension,
-- x would have had the type Integer instead of Num a => a.
{-# LANGUAGE NoMonomorphismRestriction #-}

module CompilerFlags where

-- @ LANGUAGE NoMonomorphismRestriction
-- try toggle comment on line 3 then hover on x to see effects
x = 1
