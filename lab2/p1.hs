module Main where

data BinaryTree = EmptyTree
                  | Node Integer BinaryTree BinaryTree
                    deriving (Eq, Show)

emptyTree :: BinaryTree
emptyTree = EmptyTree

infixl 7 `insert`                  
insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree val = Node val EmptyTree EmptyTree
insert (Node nval lt rt) val
    | val < nval  = Node nval (insert lt val) rt
    | val > nval  = Node nval lt (insert rt val)
    | val == nval = error "Trying to insert duplicate key"
 
infixl 7 `remove` 
remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree val = error "Value not founded"
remove (Node nval lt rt) val
    | val < nval = Node nval (remove lt val) rt
    | val > nval = Node nval lt (remove rt val)
    | lt == EmptyTree && rt == EmptyTree = EmptyTree
    | lt /= EmptyTree && rt == EmptyTree = lt
    | lt == EmptyTree && rt /= EmptyTree = rt
    | otherwise = Node maxval ltNoMax rt
        where (Node maxval _ _) = findMax lt
              ltNoMax           = removeMax lt

findMax :: BinaryTree -> BinaryTree
findMax (Node val lt EmptyTree) = Node val lt EmptyTree
findMax (Node val lt rt)        = findMax rt

removeMax :: BinaryTree -> BinaryTree
removeMax (Node val lt EmptyTree) = lt
removeMax (Node val lt rt)        = Node val lt (removeMax rt)

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree val = False
containsElement (Node nval lt rt) val 
    | val < nval = containsElement lt val
    | val > nval = containsElement rt val
    | otherwise  = True
    
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree val = error "No value founded"
nearestGE (Node nval lt rt) val
    | val > nval  = nearestGE rt val
    | val <= nval = nval
    
treeFromList :: [Integer] -> BinaryTree
treeFromList xs = foldl insert emptyTree xs
    
listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree (Node nval lt rt) = listFromTree lt ++ [nval] ++ listFromTree rt
    
main :: IO ()
main = putStrLn ""