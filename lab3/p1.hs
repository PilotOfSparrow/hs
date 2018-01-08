module Main where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

instance Eq WeirdPeanoNumber where
    (==) Zero     Zero     = True
    (==) (Succ a) (Succ b) = a == b
    (==) (Pred a) (Pred b) = a == b
    (==) _         _       = False
    (/=) a         b       = not (a == b)

instance Show WeirdPeanoNumber where
    show Zero     = "Zero"
    show (Succ a) = "Succ (" ++ show a ++ ")"
    show (Pred a) = "Pred (" ++ show a ++ ")"
    
instance Ord WeirdPeanoNumber where
    compare (Succ x) (Succ y) = compare x y
    compare Zero     (Succ _) = LT
    compare (Succ _) Zero     = GT
    
    compare (Pred x) (Pred y) = compare x y
    compare Zero     (Pred _) = GT
    compare (Pred _) Zero     = LT
    
    compare Zero     Zero     = EQ

main :: IO ()
main = putStrLn $ "Zero == Zero = "             ++ show (Zero == Zero)             ++ "\n" ++
                  "Succ(Zero) == Succ(Zero) = " ++ show (Succ(Zero) == Succ(Zero)) ++ "\n" ++
                  "Pred(Zero) == Pred(Zero) = " ++ show (Pred(Zero) == Pred(Zero)) ++ "\n" ++
                  "Pred(Zero) == Succ(Zero) = " ++ show (Pred(Zero) == Succ(Zero)) ++ "\n" ++
                  "Zero       /= Succ(Zero) = " ++ show (Zero /= Succ(Zero))       ++ "\n" ++
                  "Pred(Zero) /= Succ(Zero) = " ++ show (Pred(Zero) /= Succ(Zero)) ++ "\n" ++
                                                   show (Succ(Succ(Succ(Zero))))   ++ "\n" ++
                                                   show (Pred(Pred(Pred(Zero))))   ++ "\n"