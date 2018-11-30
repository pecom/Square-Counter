import Data.Set as Set
import Data.List

twoadic :: (Integral a) => a -> a
twoadic x
	| even == 1		= 0
	| even == 0 	= 1 + let k = x `div` 2 in twoadic k
	where even = x `mod` 2

threeadic :: (Integral a) => a -> a
threeadic x
	| threeven == 0	= 1 + let k = x `div` 3 in threeadic k
	| otherwise 	= 0
	where threeven = x `mod` 3
-- Just started learning about p-adics in class so it seemed topical to use them here
-- Finds the power of 3 in the prime factorization recursively
-- Can't use log since powers of 3 would mess up the 2-adic and powers of 2 would mess up the 3-adic

powerBump :: Int -> [Int]
powerBump x
	| x == 0		= [1]
	| otherwise		= [x-1, x+1]

-- Get the nearby powers (\pm 1) and don't go to negatives

algo :: Int -> [Int]
algo k =
	let twop = twoadic k;
		threep = threeadic k
	in [(2^l)*(3^threep) | l <- (powerBump twop)] ++ [(2^twop)*(3^m) | m <- (powerBump threep)]

-- For any number in our grid (i.e. has powers of 2 and 3 only), we get the, usually, 4 neighbors of each number by
-- changing the power of 2 and then changing the power of 3

cleanNeighbors :: [Int] -> [Int]
cleanNeighbors x = 
	let set2 = Set.fromList (concat [algo k | k <- x]);
		set1 = Set.fromList x
	in Set.toList (Set.difference set2 set1)

-- Given a list of numbers, find the neighbors for all of the numbers excluding numbers already in the given list

neighborList :: [Int] -> [[Int]]
neighborList k = [k ++ [l] | l <- cleanNeighbors k ]

-- Given a list, find the neighbors, and then make a series of new lists with each one having a single new neighbor
-- [6] -> [[6,12], [6,18]]

orderlessList :: (Ord a) => [a] -> [a] -> Bool
orderlessList a b = not (sort a == sort b)

-- We want something closer to sets than lists so using this equality
-- orderlessList [6,12] [12,6] is True (we don't care about order)

filterList :: (Ord a) => [[a]] -> [[a]]
filterList []		= []
filterList (x:xs)	= x : filterList (Data.List.filter (orderlessList x) xs)

-- Filter out duplicates from whatever list of lists we're given

fullChain :: [[Int]] -> [[Int]]
fullChain l = filterList (concat [neighborList k | k <- l])

-- Given a list of lists, get the neighbor list for each one, concatenate them, then remove any duplicates

intStep :: Int -> Int -> [[Int]]
intStep x k
	| x == 1		= [[k]]
	| otherwise		= fullChain (intStep (x-1) k)

-- Used to start at 1 but now at k since we want to be able to move in all 4 directions. "Symmetry" didn't work/I couldn't
-- get it to work. So each iteration puts k at the center of a (2n+1)x(2n+1) grid with powers of 2 on one side and powers of 3 on the other

calculate :: Int -> Int
calculate k =
	let seed = (2^(k-1))*(3^(k-1))
	in length(intStep k seed)

-- Final function to calculate the number of possible arrangements. 