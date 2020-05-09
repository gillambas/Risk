module RiskGame
  (Battlefield (..)
  ,invade
  ,invasionOutcome
  ,MonadGame (..)
  ,Outcome (..)
  )
where

import Control.Monad
import Data.List
import Data.Ord


type Army = Int
data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Eq,Ord,Show)


class Monad m => MonadGame m where
  roll :: m Int


battle :: MonadGame m => Battlefield -> m Battlefield
battle field = do
  attackDice <- replicateM (nAttackRolls field) roll
  defendDice <- replicateM (nDefenceRolls field) roll
  
  let attackDiceSort = sortOn Down attackDice
  let defendDiceSort = sortOn Down defendDice
  
  let matchups = zip attackDiceSort defendDiceSort
  
  let nDefendersLost = length $ filter (\(dA,dD) -> dA>dD) matchups
  let nAttackersLost = length $ filter (\(dA,dD) -> dD>=dA) matchups
  
  let attackersLeft = (attackers field) - nAttackersLost
  let defendersLeft = (defenders field) - nDefendersLost

  return $ Battlefield {attackers=attackersLeft, defenders=defendersLeft}


invade :: MonadGame m => Battlefield -> m Battlefield
invade (Battlefield a 0) = return $ Battlefield a 0
invade (Battlefield 0 d) = return $ Battlefield 0 d
invade (Battlefield 1 d) = return $ Battlefield 1 d
invade field = (battle field) >>= invade


nAttackRolls :: Battlefield -> Int
nAttackRolls (Battlefield allAttackers _) = let remainingAttackers = allAttackers - 1 in 
                                            if remainingAttackers > 3 then
                                              3
                                            else
                                              remainingAttackers  

nDefenceRolls :: Battlefield -> Int
nDefenceRolls (Battlefield _ allDefenders) = if allDefenders < 2 then
                                               allDefenders
                                             else
                                               2


data Outcome = AttackWins | DefenceWins | UnfinishedInvasion deriving (Eq,Ord,Show)

invasionOutcome :: Battlefield -> Outcome
invasionOutcome (Battlefield _ 0) = AttackWins
invasionOutcome (Battlefield 0 _) = DefenceWins
invasionOutcome (Battlefield 1 _) = DefenceWins
invasionOutcome _                 = UnfinishedInvasion