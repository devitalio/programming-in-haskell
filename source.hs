-- double x = x + x
quad x = double (double x)
fact n = product [1..n]
avg ns = sum ns `div` length ns
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]
last1 xs = head (reverse xs)
last2 xs = xs!!(length xs - 1)
init1 xs = reverse(tail (reverse xs))
init2 xs = take (length xs - 1) xs
add x y  = x + y

-- second xs :: [a] -> a
second xs = head (tail xs)

-- (a,b) -> (b,a)
swap (x,y) = (y,x)

-- pair x y :: a -> b -> (a,b)
pair x y = (x,y)

-- double x :: Int -> Int
double x = x*2

-- palindrome xs :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- twice f x :: (a -> a) -> a -> a
{-
twice is a function that takes a function that takes a and returns a 
and returns a function that takes a and returns a
-}
twice f x = f (f x)


-- Chapter 4
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
  where half = (length xs `div` 2);

halve2 :: [a] -> ([a],[a])
halve2 xs = splitAt middle xs
  where middle | even (length xs) = length xs `div` 2
               | otherwise        = 0;
       
halve3 xs = if even (length xs) then splitAt middle xs
            else error "Not even length"
            where middle = (length xs `div` 2)
{-
Consider a function safetail:: [a]->[a] that behaves as the library 
function tail, except that safetail maps the empty list to itself, 
whereas tail produces an error in this case. Define safetail using:
(a) a conditional expression;
(b) guarded equations;
(c) pattern matching.
Hint: make use of the library function null.
-}     
safetail :: [a] -> [a]
safetail xs = if null xs then []
              else tail xs
              
safetail2 xs | null xs = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]             
safetail3 [] = []
safetail3 (xs) = tail xs

{-
3.In a similar way to /\, show how the logical disjunction operator \/
can be defined in four different ways using pattern matching.
-}

--(||) :: Bool -> Bool -> Bool
--True || True = True
--True || False = True
--False || True = True
--False || False = False

--(||) :: Bool -> Bool -> Bool
--False || False = False
--_ || _ = True

--(||) :: Bool -> Bool -> Bool
--False || b = b
--True || _ = True

--(||) :: Bool -> Bool -> Bool
--b || c | b == c = b
--       | otherwise = True
       
{-
4.Redefine the following version of the conjunction operator using 
conditional expressions rather than pattern matching:
True /\ True = True
_ /\ _ = False
-}

{-
(&&) :: Bool -> Bool -> Bool
a && b =  if a then 
            if b then True
            else False
          else False
-}        

{-
5.Do the same for the following version, and note the difference in 
the number of conditional expressions required:
True /\ b = b
False /\ _ = False
-}
(&&) :: Bool -> Bool -> Bool
a && b =  if a then b else False

{-
6.Show how the curried function definition mult x y z = x*y*z can be
understood in terms of lambda expressions.
-}
--mult a :: Int a => a -> a -> a -> a
mult = \x -> (\y -> (\z -> x * y * z ))