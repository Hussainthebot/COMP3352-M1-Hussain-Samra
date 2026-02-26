module CPasses.C0 where

data C0 = Program [(String, Tail)]
  deriving (Eq, Show)

data Tail
  = Return Exp
  | Seq Stmt Tail
  deriving (Eq, Show)

data Stmt
  = Assign String Exp
  deriving (Eq, Show)

data Exp
  = Atm Atom
  | Add Atom Atom
  | Sub Atom 
  | Read
  deriving (Eq, Show)

data Atom
  = Int Int
  | Var String
  deriving (Eq, Show)