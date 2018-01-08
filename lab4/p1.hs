data FunMonad a = FunMonad { fun :: () -> a }

instance (Show a ) => Show (FunMonad a) where
    show (FunMonad a) = "FunMonad " ++ (show $ a ())

instance Functor FunMonad where
    fmap f (FunMonad x) = FunMonad (\() -> f (x ()))

instance Applicative FunMonad where
    pure x = FunMonad $ \() -> x
    FunMonad a <*> FunMonad b = FunMonad $ \() -> (a ()) (b ())

instance Monad FunMonad where
    return a = FunMonad (\() -> a)
    m >>= k = k (fun m ())
    fail = error


foo = FunMonad $ \() -> 101
o m = FunMonad $ \() -> 14 * m 

example = foo >>= o    -- FunMonad 1414