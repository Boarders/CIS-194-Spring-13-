
--tower of Hanoi with 3 pegs

type Peg a = a
type Move a =  (Peg a, Peg a)

threeHanoi :: Integer -> Peg a -> Peg a -> Peg a-> [Move a]
threeHanoi 1 x y z = [(x,z)]
threeHanoi 2 x y z = [(x,y), (x,z), (y,z)]
threeHanoi n x y z = (threeHanoi (n-1) x z y) ++ [(x,z)] ++ (threeHanoi (n-1) y x z)

{- Note this strategy works and is optimal i.e. it transfers all of the disks to the required peg and any strategy must minimally transfer the first (n-1) pegs, then the last peg and then the other (n-1). Hence we get that that the number of moves required satisfies:

H_1 = 1
H_n = 1 + 2*H_{n-1}

The first few values of this are: 1, 3, 7, 15 , ... and in general we get that
H_n = 2^n-1
This is easy to see if we define H'_n = H_{n}+1. Then we get:
H'_1 = 2
H'_n = H_n + 1= 2*(H{n-1}) + 2 = 2*(H'_{n-1}-1) + 2 = 2*H'_{n+1} -}

-- tower of Hanoi with 4 pegs, most naive implementation which uses the Frame-Stewart algorithm. This says one should first move k disks to an intermediary peg,
-- then move the remaining (n-k) to the target peg and then move the k disks to the target peg where k is chosen so that this is minimal. It is conjectured but
-- not proven that this is an optimal strategy (i.e. that this strategy always leads to a minimal number of moves). 

fourHanoi :: Integer -> Peg a -> Peg a-> Peg a -> Peg a-> [Move a]
fourHanoi 0 x y z w = []
fourHanoi 1 x y z w = [(x,w)]
fourHanoi 2 x y z w = [(x,y),(x,w),(y,w)]
fourHanoi 3 x y z w = [(x,y), (x,z),(x,w),(z,w),(y,w)]
fourHanoi n x y z w = fst (a,b) where
                                  (a,b)= minLengthElement l
                                  l=map f [1..n-1]
                                  f k = g (fourHanoi (n-k) x y w z ++ threeHanoi k x y w ++ fourHanoi (n-k) z x y w) where
                                  g xs = (xs, length xs)
                                  
minLengthElement :: [(a,Int)] -> (a,Int)
minLengthElement []= error "empty list"
minLengthElement [(x,n)]= (x,n)
minLengthElement ((x,n) : xs) | n < snd(minLengthElement xs) = (x,n)
                              | otherwise = minLengthElement xs

                              
--length fourHanoi 15 "a" "b" "c" "d" does give 29 as it should but is quite slow


--Here is an implementation (essentially from: https://stackoverflow.com/a/3615658/8014727) with a specific k, though a general formula for which k works
--is not known:
-- s - source
-- t - target
-- i i intermediate peg

frameStewartHanoi :: Integer -> [Peg a] -> [Move a]
frameStewartHanoi 0 ps = []
frameStewartHanoi 1 (s:t:i:ps) = [(s,t)]
frameStewartHanoi 2 (s:t:i:ps) = [(s,i), (s,t), (i,t)]
frameStewartHanoi n [s,t,i] = (frameStewartHanoi (n-1) [s,i,t]) ++ [(s,t)] ++ (frameStewartHanoi (n-1) [i,s,t])
frameStewartHanoi n (s:t:i:ps) =
  frameStewartHanoi k (s:i:t:ps) ++
  frameStewartHanoi (n-k) (s:t:ps) ++
  frameStewartHanoi k (i:s:t:ps)
    where k = n-(round (sqrt(2*n'+1))::Integer)+1
          n'=fromIntegral n

--we have chosen the k that is the best for 4 pegs, as mentioned here: https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm



