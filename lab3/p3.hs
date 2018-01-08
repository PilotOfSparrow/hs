newtype PSet a  = PSet  { contains :: (a -> Bool) }
newtype PSet' a = PSet' { contains' :: (a -> Bool) }

instance Monoid (PSet a) where
    mempty = PSet (\x -> False)
    mappend (PSet x) (PSet y) = PSet (\z -> (||) (x z) (y z))

instance Monoid (PSet' a) where
    mempty = PSet' (\x -> False)
    mappend (PSet' x) (PSet' y) = PSet' (\z -> (&&) (x z) (y z))

instance Functor PSet where
    fmap f (PSet a) = PSet (\x -> x == f a)