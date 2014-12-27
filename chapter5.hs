import Data.Char

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [c | c <- xs, x==c]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let (  (let2int c + n) `mod` 26  )
          | otherwise = c
          
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]


positions :: Float -> [Float] -> [Int]
positions n xs = [ x | (x,y) <- zip [0..(length xs)] xs, n==y ]

percent :: Int -> Int -> Float
percent n m = ( fromIntegral n / fromIntegral m ) * 100

freqs :: String -> [Float]
freqs xs = [ percent ( count x xs) n | x <- ['a'..'z']  ]
            where n = lowers xs

-- Chi square of two lists            
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs++take n xs

crack :: String -> String
crack xs = encode (-factor) xs
          where factor = head( positions (minimum chitab) chitab)
                chitab=[ chisqr (rotate n table') table |n <-[0..25] ]
                table' =freqs xs
                
{-
1. Using a list comprehension, give an expression that calculates the sum
1^2+2^2+...100^2 of the first one hundred integer squares.
-}
tt = sum [ x^2 | x <- [1..100] ]

{-
2.In a similar way to the function length, show how the library function
replicate::Int->a->[a] that produces a list of identical elements can
be defined using a list comprehension. For example:
>replicate3True
[True,True,True]
-}
replicate::Int->a->[a]
replicate n x = [ x | _ <- [1..n] ]   

{-
3.Atriple(x, y, z) of positive integers is pythagorean if
x^2+y^2=z^2. Using a list comprehension, define a function
pyths::Int->[(Int,Int,Int)] that returns the list of all pythagorean 
triples whose components are at most a given limit. For example:
>pyths10
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}
pyths::Int->[(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y<-[1..n], z<-[1..n] , x^2 + y^2 == z^2]

{-
4.A positive integer is perfect if it equals the sum of its factors, 
excluding the number itself. Using a list comprehension and the 
function factors, define a function 
perfects::Int->[Int] that returns the list of all perfect numbers
up to a given limit. For example:
>perfects 500
[6,28,496]
-}

-- First define factors, that exludes the number itself
factors_ :: Int->[Int]
factors_ n = [x | x<-[1..n-1], n `mod` x == 0]

-- define function that drops
perfects::Int->[Int]
perfects n = [ x | x <- [1..n], sum (factors_ x) == x ]

{-
5.Show how the single comprehension 
[ (x,y)| x<-[1,2,3], y<-[4,5,6] ] with two generators can be re-expressed
using two comprehensions with single generators. 
Hint: make use of the library function concat and nest one 
comprehension within the other.
-}
ttt = [ (x,y)| x<-[1,2,3], y<-[4,5,6] ]
--ttt' = [ concat[ (zip [1,1,1] [4,5,6]), (zip [2,2,2][ 4,5,6 ]), (zip [3,3,3] [4,5,6 ]) ]]
ttt' = concat[    [ (x,y)|y<-[4,5,6] ] | x <-[1,2,3] ]

{-
6.Redefine the functionpositionsusing the function find.
-}

-- find
find :: Eq a => a ->[(a,b)] -> [b]
find k t = [v |(k',v) <- t, k==k']

positions' :: Eq a => a->[a]->[Int]
positions' x xs = [i | i <- find x (zip xs [0..n])]
                  where n = length xs -1


{-
7.The scalar productof two lists of integers xs and ys of length n is
given by the sum of the products of corresponding integers:
n-1
E   (xsi * ysi)
i=0

In a similar manner to the function chisqr, show how a list 
comprehension can be used to define a function 
scalarproduct:: [Int]->[Int]->Int 
that returns the scalar product of two lists. For example:
>scalarproduct[1,2,3] [4,5,6]
32
-}
scalarproduct:: [Int]->[Int]->Int 
scalarproduct xs ys = sum[ (x * y) | (x,y) <- zip xs ys]























