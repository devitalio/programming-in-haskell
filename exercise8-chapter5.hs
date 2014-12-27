{-- 

Modified Caesar Cipher to handle capitals

--}

import Data.Char

-- Asume capitals have the same freq
table :: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]


lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [c | c <- xs, x==c]

isBlank :: Char -> Bool
isBlank ' ' = True
isBlank '\t' = True
isBlank _ = False 

let2int :: Char -> Int
let2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | not (isBlank c) = int2let (  (let2int c + n) `mod` ( ord 'z' - ord 'A' )  )
          | otherwise = c
          
          
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

------------------------------------------------
-- Crack
------------------------------------------------
positions :: Float -> [Float] -> [Int]
positions n xs = [ x | (x,y) <- zip [0..(length xs)] xs, n==y ]

percent :: Int -> Int -> Float
percent n m = ( fromIntegral n / fromIntegral m ) * 100

alphas :: String -> Int
alphas cs = length [c | c <- cs, isAlpha c]

freqs :: String -> [Float]
freqs xs = [ percent ( count x xs) n | x <- ['a'..'z']  ]
            where xs' = map toLower xs
                  n = alphas xs

-- Chi square of two lists            
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs++take n xs

crack :: String -> String
crack xs = encode (-factor) xs
          where factor = head( positions (minimum chitab) chitab)
                chitab =[ chisqr (rotate n table') table |n <-[0..25] ]
                table' = freqs xs
                
                
                