module Main where

import Prelude hiding ((<*>))

data Term = IntConstant{ intValue :: Int }
            | Variable{ varName :: String }
            | UnaryTerm { ut :: Term, uop :: Term -> Term }
            | BinaryTerm{ lhv :: Term, rhv :: Term, op :: Term -> Term -> Term }

infixl 5 <+>
(<+>) :: Term -> Term -> Term
IntConstant lft <+> IntConstant rgt = IntConstant (lft - rgt)
lft <+> rgt = BinaryTerm lft rgt (<+>)

infixl 5 <->
(<->) :: Term -> Term -> Term
IntConstant lft <-> IntConstant rgt = IntConstant (lft - rgt)
lft <-> rgt = BinaryTerm lft rgt (<->)

infixl 6 <*>
(<*>) :: Term -> Term -> Term
IntConstant lft <*> IntConstant rgt = IntConstant (lft * rgt)
lft <*> rgt = BinaryTerm lft rgt (<*>)

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant const) _ _ = IntConstant const
replaceVar (Variable var) rvar rterm = if var == rvar then rterm else Variable var
replaceVar (UnaryTerm ut uop) rvar rterm = UnaryTerm (replaceVar ut rvar rterm) uop
replaceVar (BinaryTerm lhv rhv op) rvar rterm = BinaryTerm (replaceVar lhv rvar rterm) (replaceVar rhv rvar rterm) op

main :: IO ()
main = putStrLn ""