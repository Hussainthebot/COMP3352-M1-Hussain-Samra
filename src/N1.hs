module N1 where

import Data.Int (Int64)

-- ===== N1 Abstract Syntax =====

data Exp
  = Int Int64
  | Var String
  | Read
  | Negate Exp
  | Add Exp Exp
  | Let String Exp Exp
  deriving (Eq, Show)

newtype Program = Program Exp
  deriving (Eq, Show)
