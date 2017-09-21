-- exercise 1: wholemeal programming

fun1 :: [Integer] -> Integer
fun1 = foldr (\x acc -> (if even x then (x-2)*acc else acc)) 1


fun2 :: Integer -> Integer
fun2 = sum.takeWhile(>1).(iterate (\n-> (if even n then (n `div` 2) else (3*n+1) ) ) )

--exercise 2: folding with trees

data Tree a = Leaf |
              Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\x acc -> balancedInsert x acc) Leaf

height :: Tree a -> Integer
height Leaf = (-1)
height (Node n _ _ _) = n

balancedInsert :: a -> (Tree a) -> (Tree a)
balancedInsert x tree = case tree of
  Leaf -> Node 0 Leaf x Leaf
  Node 0 (Leaf) y (Leaf) -> Node 1 (Node 0 Leaf x Leaf) y Leaf
  Node n (leftTree) y (rightTree)
    | (height leftTree) < (height rightTree) -> Node n (balancedInsert x leftTree) y (rightTree)
    | (height leftTree) > (height rightTree) -> Node n (leftTree) y (balancedInsert x rightTree)
    | otherwise -> Node n' (newLeftTree) y (rightTree)
    where newLeftTree = (balancedInsert x leftTree)
          n' = max (height newLeftTree) (height rightTree) + 1

-- exercise 3: More folds!

xor :: [Bool] -> Bool
xor = foldr (\b1 b2 -> case b1 of False -> b2; True -> (not b2)) False

map' :: (a-> b) -> [a] -> [b]
map' f = foldr(\x acc -> f(x) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = (foldr (flip f) base).reverse


--exercise 4: Finding primes
aboveDiag :: Ord a =>  [a] -> [a] -> [(a,a)]
aboveDiag xs ys = [(x,y) | x <- xs, y<- ys, y>=x]

filterFromList :: Eq a => [a] -> [a] -> [a]
filterFromList [] xs = xs
filterFromList (x:xs) ys = filterFromList xs (filter (/= x) ys)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map(\n-> 2*n + 1) $ filterFromList rmvList [1..n]
  where rmvList = map(\(i,j)-> i+j+2*i*j) $ aboveDiag [1..n] [1..n]

        
