-- Plus Grand Commun Diviseur
-- The greatest Common Divisor
{-
pgcd :: Integral a => (a,a) -> a
pgcd (x,y) = if x == 0 then y
             else 
                if y == 0 then x
                else
                   if (x < y) then pgcd (y-x,x) 
                   else pgcd (x-y, y)

-}

{-
pgcd :: Integral a => a -> a -> a
pgcd x y | x == 0 = y
         | y == 0 = x
         | x < y  = pgcd (y-x) x
         | otherwise = pgcd (x-y) y
-}

pgcd :: Integral a => a -> a -> a
pgcd = \x -> (\y -> if x == 0 then y
                    else if y == 0 then x
                    else if x < y  then pgcd (y-x) x
                    else pgcd (x-y) y)