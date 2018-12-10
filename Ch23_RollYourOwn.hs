module Ch23_RollYourOwn where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  -- Use 'error'
  -- _extremely_ sparingly.
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0 where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= 20 = count
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (count + 1) nextGen

-- ex01 -> Refactor rollsToGetTwenty into having
--         the limit be a function argument.
type Count    = Int
type LocalSum = Int

rollsToGetN :: Int -> StdGen -> Count
rollsToGetN n = recursive 0 0
  where recursive :: LocalSum -> Count -> StdGen -> Count
        recursive sum count gen
          | sum >= n  = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
              in  recursive (sum + die) (count + 1) nextGen

-- ex02 ->  Change rollsToGetN to recording the series
--          of die that occurred in addition to the count.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = recursive 0 0 []
  where
    recursive :: Int -> Int -> [Die]
              -> StdGen -> (Int, [Die])
    recursive sum count xs gen
      | sum >= n  = (count, xs)
      | otherwise =
        let (x, nextGen) = randomR (1, 6) gen
            die          = intToDie x
        in  recursive (sum + x) (count + 1) (die : xs) nextGen
