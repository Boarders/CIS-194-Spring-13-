
{-exercise 1, validating credit cards: In order to check a credit card number
is valid we perform a check sum. This involves the following steps:
1. Starting from the last digit we double every other digit e.g.
[1,2,3,4] -> [2,2,6,4], [1,2,3]->[1,4,3]
2. We then sum the digits e.g. digitsSum [1,12,4] = 1 + 1 + 2 + 4
3. If the sum of the digits is equal to 0 mod 10 i.e. if the unit digit is
equal to zero then the card is valid.-}

import Data.Char

--toDigits converts a number to a list of its digits
toDigits :: Integer -> [Integer]
toDigits n | n <= 0 = []
           | otherwise = (map ((\n->n-48).fromIntegral.ord). show) n

-- using only Prelude
toDigitsRev' :: Integer -> [Integer]
toDigitsRev' n  | n<= 0 = []
                | otherwise = (n `mod` 10) : (toDigitsRev' (n `div` 10))

toDigits' :: Integer -> [Integer]
toDigits' = reverse. toDigitsRev'

   

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse.toDigits

--doubles every other number starting from the last digit
doubleEveryOther :: [Integer]->[Integer]
doubleEveryOther  = reverse.doubleEveryOtherStart.reverse

--doubles every other number starting from the first digit
doubleEveryOtherStart :: [ Integer] -> [Integer]
doubleEveryOtherStart [] = []
doubleEveryOtherStart [x] = [x]
doubleEveryOtherStart (x:y:xs) = x:2*y:(doubleEveryOtherStart xs)

--perform the digit sum on a list of one and two digit numbers
sumDigits :: [Integer] -> Integer
sumDigits = foldr (\x y -> (x `div` 10) + (x `mod` 10) + y) 0

validate :: Integer -> Bool
validate n = (((\n->n `mod` 10).sumDigits.doubleEveryOther.toDigits) n) == 0

test1 = (validate 4012888888881881) == True
test2 = (validate 4012888888881882) == False



