{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons (Emp name fun) (GL ls n) = GL (ls ++ [Emp name fun]) (n+fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL ls n) (GL ls' n') = (GL (ls++ls') (n+n'))

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL ls n) (GL ls' n')
  | n>=n' = (GL ls n)
  | otherwise = (GL ls' n')

treeFold :: (a->[b]->b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a xs) = f a recurseChildren
  where recurseChildren = fmap (treeFold f) xs


-- note: the second argument gives the best guest list of each of
-- the subtrees both including and not including the boss of each
-- subtree. This computes updates the list with a new boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList,GuestList)
nextLevel boss sublists = (incBoss,noBoss)
  where incBoss = glCons boss noBossSubLists
        noBoss = foldr (\(x,y) acc -> moreFun (moreFun x y) acc)
                       mempty sublists
        noBossSubLists = foldr (\(x,y) acc -> moreFun y acc)
                               mempty sublists

maxFun :: Tree Employee -> GuestList
maxFun employeeTree = moreFun incBoss noBoss
  where (incBoss, noBoss) = treeFold (nextLevel) employeeTree

showGuests :: GuestList -> String
showGuests (GL xs n) = "Total fun: " ++ show n ++ "\n" ++ guests
  where guests = init $ unlines employees
        employees = map empName xs

main :: IO ()
main = do
  companyString<-readFile "company.txt"
  let companyTree = (read companyString) :: Tree Employee
  let guestList = maxFun companyTree
  putStrLn $ showGuests guestList
                       
       
