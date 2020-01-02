module Simulation 
  (simulation)
where 

import Control.Monad
import Data.Ratio
import RiskGame
import System.Random


instance MonadGame IO where
  roll = randomRIO (1,6)


simulation :: Battlefield -> Int -> IO Rational
simulation initialField nSims = do
  finalFields <- replicateM nSims (invade initialField)
  let outcomes = map invasionOutcome finalFields
  let nWins = length $ filter (==AttackWins) outcomes
  let ratio = (toInteger nWins) % (toInteger nSims)
  return ratio
