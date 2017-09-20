-- code golf

module Golf where

import Control.Monad
import Data.List

-- ex 1 skips: a function which takes a list and returns a list of lists where the
-- nth list consists of every n elements of the original list

skips :: [a] -> [[a]]
skips xs = [[x | (x,n) <- xs', n `mod` k == 0] | k<-[1..l]]
  where xs'= zip xs [1..]
        l=length xs

-- ex 2 local maxima: find all local maxima
-- def: an element is a local maximum if it is greater than both of its neighbours

localMaxima :: [Integer] -> [Integer]
localMaxima = (map g).(filter (\((x,y),(z,w)) -> (y>=x) && (z>=w))).f.f  
  where f=ap zip tail
        g = fst.snd


-- ex 3 Histogram: Take a list of integers between 0 and 9 inclusive and print a
-- histogram of the number of times each one appears.

histogram :: [Integer] -> String
histogram = ( ++"\n==========\n0123456789\n").unlines.transpose.table

printHistogram :: [Integer] -> IO ()
printHistogram = putStr.histogram


groupByNumber :: [Integer] -> [(Integer, Int)]
groupByNumber = (map (\xs-> (head xs, (length xs)-1) )) . group . sort.((++ [0..9]))

table :: [Integer] -> [String]
table = (map (\(x,k)-> f 10 k)) . groupByNumber
   
f :: Int -> Int -> String
f n k = (replicate (n - k) ' ') ++ (replicate k '*')
 



