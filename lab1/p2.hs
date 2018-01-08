module Main where

-- Taylor Series sin(x) eval

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial(n - 1)

tsinTerm :: (Num a, Fractional a) => a -> Integer -> a
tsinTerm x n = (x^oddTerm / fromIntegral(factorial oddTerm)) * (-1)^(n - 1)
    where oddTerm = 2 * n - 1
    
tsin x prec = sum[tsinTerm x n | n <- [1..prec]]

-- Euclidean algorithm for biggest mutual div

gcd' :: Integral f => f -> f -> f
gcd' z 0 = z
gcd' z b = gcd' b (z `mod` b)

-- Exponentiation by squaring

expS :: Integer -> Integer -> Integer
expS a 0 = 1
expS a b
    | odd b = a * expS a (b - 1)
    | otherwise = p * p
        where p = expS a (b `div` 2)

main :: IO ()
main = putStrLn ""