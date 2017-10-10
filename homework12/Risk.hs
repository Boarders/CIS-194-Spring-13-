{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


-- assume in this function a and d correspond to precisely the units in battle
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  attackRolls <- replicateM a die
  defendRolls <- replicateM d die
  let sortAttack = sort attackRolls
  let sortDefence = sort defendRolls
  let outcome = zipWith (>) sortAttack sortDefence
  let a' = length $ takeWhile (==True) outcome
  let d' = length $ dropWhile (==True) outcome
  let numbattles = min a d
  return $ Battlefield (a-numbattles+a') (d-numbattles + d')


invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield @ (Battlefield a d) = do
  if (a >= 2 && d >= 1) then do 
    updateBattlefield <- battle (Battlefield a d)
    invade updateBattlefield
  else do
    return battlefield

successBattle :: Num a => Battlefield -> a
successBattle (Battlefield a d)
  | a == 1 = 0
  | d == 0 = 1
  | otherwise = error "battle not over"
  

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield @ (Battlefield a d) = do
  battles <- replicateM 1000 (battle battlefield)
  let outcomes = map successBattle battles
  let prob = (/) (sum outcomes) 1000.0
  return prob


-- compute the exact probability of success
exactSuccessProb :: Battlefield -> Double
  

  
