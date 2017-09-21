{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where

import qualified ExprT as Exp
import Parser
import qualified StackVM as St
import qualified Data.Char as Ch
import qualified Data.Map as M
import Control.Monad


eval :: Exp.ExprT -> Integer
eval (Exp.Lit n) = n
eval (Exp.Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Exp.Mul exp1 exp2) = (eval exp1) * (eval exp2)

evalStr :: String -> Maybe Integer
evalStr st = case maybeExp of
              (Just exp) -> Just (eval exp)
              Nothing -> Nothing
  where maybeExp = parseExp Exp.Lit Exp.Add Exp.Mul st

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Exp.ExprT where
  lit = Exp.Lit 
  add  = Exp.Add
  mul = Exp.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n | n <= 0 = False
        | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax n) (MinMax m) = (MinMax (max n m))
  mul (MinMax n) (MinMax m) = (MinMax (min n m))


instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 n) (Mod7 m) = Mod7 ((n+m) `mod` 7)
  mul (Mod7 n) (Mod7 m) = Mod7 ((n*m) `mod` 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

--exercise 5

instance Expr (St.Program) where
  lit n = [St.PushI n]
  add xs ys = xs ++ ys ++ [St.Add]
  mul xs ys = xs ++ ys ++ [St.Mul]

compile :: String -> Maybe (St.Program)
compile (x:xs)
  | Ch.isDigit x = fmap ((:) (St.PushI x')) (compile xs)
  | x == '+' =  fmap ((:) St.Add) (compile xs)
  | x == '*' = fmap ((:)  St.Mul) (compile xs)
  | otherwise = Nothing
  where x' = read [x] :: Integer

class HasVars a where
  var :: String -> a

data VarExpT = 
  Lit Integer | 
  Add VarExpT VarExpT | 
  Mul VarExpT VarExpT |  
  Var String
  deriving (Show, Eq)

instance Expr VarExpT where
  lit n = Lit n
  add = Add
  mul = Mul

instance HasVars VarExpT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = const $ Just n
  add f g = (\mp -> liftM2 (+) (f mp) (g mp) )
  mul f g = (\mp -> liftM2 (*) (f mp) (g mp) )

withVars :: [(String, Integer) ] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

testVars1 = withVars [("x",6)] $ add (lit 3) (var "x")
testVars2 = withVars [("x",6)] $ add (lit 3) (var "y")
testVars3 = withVars [("x",6), ("y",3)] $ mul (var "x") (add (var "y") (var "x"))
  




