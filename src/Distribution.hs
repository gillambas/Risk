module Distribution
  (exactProbability)
where


import qualified Data.Map as Map
import           Data.Ord
import           Data.Ratio
import           RiskGame


data Dist a = D { unD :: [(a,Rational)] } deriving Show


instance Functor Dist where
  fmap f (D d) = D [(f x,p) | (x,p) <- d]

instance Applicative Dist where
  (D df) <*> (D d) = D [(f x,p*q) | (x,p) <- d, (f,q) <- df]
  pure outcome = D [(outcome,1 % 1)] 

instance Monad Dist where
  (D d) >>= f = D [(y,p*q) | (x,p) <- d, (y,q) <- unD (f x)]
  return outcome = D [(outcome,1 % 1)]

instance MonadGame Dist where
  roll = D $ zip [1 .. 6] (replicate 6 (1 % 6))


normalise :: Ord a => Dist a -> Dist a
normalise = D . Map.toList . Map.fromListWith (+) . unD

probOfEvent :: Eq a => a -> Dist a -> Rational
probOfEvent event = sum . map snd . filter ((== event) . fst) . unD


exactProbability :: Battlefield -> Rational
exactProbability initialField = probWin
  where 
    finalFieldDist = normalise $ invade initialField
    outcomesDist = normalise $ fmap invasionOutcome finalFieldDist
    probWin = probOfEvent AttackWins outcomesDist