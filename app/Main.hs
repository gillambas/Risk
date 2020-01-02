module Main
  (main)
where

import Distribution
import RiskGame
import Simulation


main :: IO ()
main = do 
  putStrLn "Enter number of armies of attacking player: "
  nAttackers <- read <$> getLine 
  
  putStrLn "Enter number of armies of defending player: "
  nDefenders <- read <$> getLine 

  putStrLn "Enter number of simulations: "
  nSims <- read <$> getLine

  let field = Battlefield {attackers=nAttackers, defenders=nDefenders}
  
  if (nAttackers > 3 || nDefenders > 2) then
    putStrLn "Too many armies to calculate exact probability!"
  else do
    let probExact = exactProbability field
    putStrLn $ "Exact probability: " <> show probExact <> " (" <> show (fromRational probExact) <> ")"
  
  probSim <- simulation field nSims
  putStrLn $ "Probability by Monte Carlo: " <> show probSim <> " (" <> show (fromRational probSim) <> ")"