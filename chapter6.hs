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
