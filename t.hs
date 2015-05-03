import Data.List (transpose, splitAt )

-- type N Int

s :: Int -> [int] -> [[int]]
s n [] = []
s n xs = (take n xs) : (s n $ drop n xs)


-- ssa :: Int -> [Int] -> [[Int]]
-- ssa n xs = j xs1 $ ss n xs2
--   where (xs1, xs2) = splitAt n xs
--         j [] xss = []
--         j xs [] = [ [x] | x <- xs ]
--         j (x:xs) (rest1: rests) = (x: rest1) : (j xs rests)

ss :: Int -> Int -> [Int] -> [[Int]]
ss n 0 xs = []
ss n i xs = (nth n xs): ss n (i-1) (tail xs)



-- this version of nth does not do extra consing, but does call a lot
-- of functions.
nth' :: Int -> [a] -> [a]
nth' n xs = f 0 n xs
    where f _ _ [] = []
          f 0 n (x:xs) = x: (f 1 n xs) 
          f i n (x:xs) = if i < n then f (i+1) n xs else f 0 n (x:xs)

-- surprisingly, this version of nth is about twice as fast, even
-- though it involves a zip and a cons.
nth'' :: Int -> [a] -> [a]
nth'' n xs = [x | (x, i) <- zip xs [0..], i `mod` n == 0 ] 

-- This version is about four times faster than the previous.
nth :: Int -> [a] -> [a]
nth n [] = []
nth n xs = (head xs1) : (nth n xs2)
  where (xs1, xs2) = splitAt n xs



