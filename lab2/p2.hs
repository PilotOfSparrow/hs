module Main where

mFoldl f z [] = z
mFoldl f z (x:xs) = mFoldl f (f z x) xs

mFoldr f z [] = z
mFoldr f z (x:xs) = f x (mFoldr f z xs)

mMap :: (a -> b) -> [a] -> [b]
mMap _ [] = []
mMap f xs = mFoldl (\z p -> z ++ [f p]) [] xs

mFlatMap :: (a -> [b]) -> [a] -> [b]
mFlatMap _ [] = []
mFlatMap f xs = mFoldl (\z p -> z ++ f p) [] xs

mConcat :: [a] -> [a] -> [a]
mConcat l [] = l
mConcat [] r = r
mConcat l r  = mFoldl (\z p -> z ++ [p]) l r

mFilter :: (a -> Bool) -> [a] -> [a]
mFilter _ [] = []
mFilter f xs = mFoldl (\z p -> if f p then z ++ [p] else z) [] xs

mMaxBy :: (a -> Integer) -> [a] -> a
mMaxBy _ [] = error "Empty list"
mMaxBy f xs = mFoldl (\b p -> if f p > f b then p else b) (head xs) xs

mMinBy :: (a -> Integer) -> [a] -> a
mMinBy _ [] = error "Empty list"
mMinBy f xs = mFoldl (\l p -> if f p < f l then p else l) (head xs) xs

mReverse :: [a] -> [a]
mReverse [] = []
mReverse xs = mFoldl (\z p -> p : z) [] xs

mElementAt :: Integer -> [a] -> a
mElementAt _ [] = error "Empty list or index out of bound"
mElementAt e xs
    | e < 0 || e >= toInteger (length xs) = error "Index out of bound"
    | otherwise = head $ mFoldl (\r i -> if i < e then tail r else if i == e then [head r] else r) xs [0..e]

mIndexOf :: String -> [String] -> Integer
mIndexOf _ [] = -1
mIndexOf s xs
    | r >= 0 = r
    | otherwise = -1
        where r = mFoldl (\i p -> if p == s && i < 0 then abs i - 1 else if i < 0 then i -1 else i) (-1) xs
    

    
main :: IO ()
main = putStrLn ""