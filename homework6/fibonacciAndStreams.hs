{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

--exercise 1: naive implementation of fibonacci numbers.
fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

fibs1 :: [Integer]
fibs1 = map (fib1) [0..]

--exercise 2: less naive implementation of fibonacci numbers.
fib2 :: Integer -> Integer
fib2 n = g 0 1 n

g :: Integer -> Integer -> Integer-> Integer
g a b 0 = a
g a b n = g b (a+b) (n-1)

fibs2 = map (fib2) [0..]

fibs3 :: [Integer]
fibs3 = 0:1:(zipWith (+) fibs3 (tail fibs3))

fib3 :: Int -> Integer
fib3 = (!!) fibs3

--exercise 3: define streams

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a str) = a : (streamToList str)

instance Show a => Show (Stream a) where
  show str = init(show lsStr) ++ ",..."
   where lsStr = take 20 (streamToList str)

--exercise 4: some basic stream tools

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a->b) -> Stream a -> Stream b
streamMap f (Cons a str) = Cons (f a) (streamMap f str)

instance Functor Stream where
  fmap = streamMap

streamFromSeed :: (a->a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- exercise 5: create some streams

nats :: Stream Integer
nats = streamFromSeed succ 0

--had to look up a hint for this: see https://codereview.stackexchange.com/a/66811
-- the ruler function goes:
-- interleave (repeat 0) (Stream (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 6...)
-- = interleave (repeat 0) (interleave 1 (2 3 2 4 2 3 2 5 2 3 ...)
-- = interleave (repeat 0) (interleave (repeat 1) (interleave (repeat 2) (...)))

ruler :: Stream Integer 
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x str1) str2 = Cons x (interleaveStreams str2 str1)


-- exercise 6: formal power series

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

scalarMult :: Integer -> Stream Integer -> Stream Integer
scalarMult n (Cons n1 str) = Cons (n*n1) (scalarMult n str)

multByX :: Stream Integer -> Stream Integer
multByX str = Cons 0 str

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons n str) = Cons (-n) (negate str)
  (+) (Cons n1 str1) (Cons n2 str2) = Cons (n1+n2) ((+) str1 str2)
  (*) (Cons n1 str1) (strm2@(Cons n2 str2)) = Cons (n1*n2) (tailMult)
    where tailMult = (scalarMult n1 str2) + (str1*strm2)

instance Fractional (Stream Integer) where
  (/) f@(Cons a str1) g@(Cons b str2) = Cons (a `div` b) (scalarMult (1 `div` b) (str1 - (f/g)*str2))

{- only to be used on those power series which are invertible. It is easy to show this is the
   power series of the form u+x*f where u is a unit. In particular power series of the form
   f(x) = 1+a_0x + a_1x^2+... are invertible. -}

{- Note that if F(x) = F_0 + F_1 x+ ... is the generating function for the fibonacci numbers
   then we have that F(x) = x + x*F(x) + x^2*F(x) and re-arranging gives:
                     F(X) = x * (1 - x - x^2)^(-1) -}

fibs4 :: Stream Integer
fibs4 = x / (1-x-x^2)

--exercise 7: fibonacci numbers via matrix multiplication
data Matrix a = Matrix a a a a

instance Show a => Show (Matrix a) where
  show (Matrix a b c d) =  (show a) ++ ", " ++ (show b) ++ "\n" ++ (show c) ++ ", " ++ (show d)

instance Functor (Matrix) where
  fmap f (Matrix a b c d) = Matrix (f a) (f b) (f c) (f d)




instance Num (Matrix Integer) where
  fromInteger n = Matrix n 0 0 n
  negate = fmap (negate)
  (+) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) = Matrix (a1+a2) (b1+b2) (c1+c2) (d1+d2)
  (*) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) =
                      Matrix (a1*a2+b1*c2) (a1*b2 + b1*d2) (c1*a2 + d1*c2) (c1*b2+d1*d2)

fibMat :: Matrix Integer
fibMat = Matrix 1 1 1 0

topRight :: Matrix a -> a
topRight (Matrix _ b _ _) = b
                              

fib5 :: Integer -> Integer
fib5 n = topRight fibPow
  where fibPow = fibMat^n

fibs5 :: [Integer]
fibs5 = map (fib5) [0..]
