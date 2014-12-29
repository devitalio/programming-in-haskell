{-
1.Define the exponentiation operator ^ for non-negative integers 
using the same pattern of recursion as the multiplication operator *,
and show how 2^3 is evaluated using your definition.
-}

--define type
-- use pow to avoid collision with Prelude
pow :: Int -> Int -> Int
pow 0 _ = 0
pow 1 _ = 1
pow n 0 = 1
pow n 1 = n 
pow x (n+1) = x * ( x `pow` n)

{- 
2 `pow` 3
2 * ( 2 `pow` 2)
2 * ( 2 * ( 2 `pow` 1))
2 * ( 2 * ( 2))
8 
-}

{-
2.Using the definitions given in this chapter, show how
length [1,2,3],
drop 3 [1,2,3,4,5], 
and init [1,2,3] 
are evaluated
-}

{-
length [1,2,3]
1 + length [2,3]
1 + 1 + lenght [3]
1 + 1 + 1 + length []
1 + 1 + 1 + 0
3

drop 3 [1,2,3,4,5]
drop 2 [2,3,4,5]
drop 1 [3,4,5]
drop 0 [4,5]
[4,5]

init [1,2,3]
1:init [2,3]
1:2:init[3]
1:2:[]
[1,2]
-}

{-
3.Without looking at the definitions from the standard prelude, 
define the following library functions using recursion.
– Decide if all logical values in a list are True:
and :: [Bool] -> Bool
– Concatenate a list of lists:
concat :: [[a]] -> [a]
– Produce a list with n identical elements:
replicate :: Int ->a->[a]
– Select the nth element of a list:
(!!) :: [a]->Int->a
– Decide if a value is an element of a list:
elem :: Eq a =>a->[a]->Bool

Note: most of these functions are in fact defined 
in the prelude using other library functions, rather than
using explicit recursion.
-}
and' :: [Bool] -> Bool
and' [] = error "Undefined"
and' [True] = True
and' (False:_) = False
and' (True:xs) = and xs

concat' :: [[a]] -> [a]
concat'  [] = []
concat'  [[]] = []
concat' [[x]] = [x] 
concat' (x:xs) = x ++ (concat' xs)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' (n+1) x = x : replicate n x

selectn :: [a] -> Int -> a
selectn [] _ = error "IndexOutOfRangeException :)"
selectn (x:_) 0 = x
selectn (_:xs) (n+1) = selectn xs n

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' z (x:xs) | z == x = True
               | otherwise = elem z xs
               
{-
4.Define a recursive function
merge::Ord aâ‡’[a]â†’[a]â†’[a] that
merges two sorted lists to give a single sorted list. For example:
>merge[2,5,6] [1,3,4]
[1,2,3,4,5,6]
Note: your definition should not use other functions on sorted lists such as
insert or isort, but should be defined using explicit recursion
-}
merge::Ord a =>[a]->[a]->[a]
merge [] [x] = [x]
merge [x] [] = [x]
merge [x] [y] | x < y = [x,y]
              | otherwise = [y,x]
merge (x:xs) (y:ys) | x < y && x < ys!!0 = x : y : merge xs ys
                    | x < y && x > ys!!0 = x : xs!!0 : y : merge xs (tail ys)
                    | y < x && y < xs!!0 = y : x : merge ys xs
                    | y < x && y > xs!!0 = y : ys!!0 : x : merge xs (tail xs)
 



