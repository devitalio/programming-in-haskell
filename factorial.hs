factorial :: Int -> Int
factorial 0 = 1
factorial (n + 1) = (n + 1) * factorial n

pred :: Int -> Int
pred 0 = 0
pred (n+1) = n