module Env
  ( Env
  , makeEnv
  , lookupEnv
  , extendEnv
  ) where

import qualified Data.Map as M

-- =====================================================
-- ENVIRONMENT (SYMBOL TABLE)
-- =====================================================
-- This module is used by COMPILER PASSES.
-- It maps names to metadata (usually other names).
-- It NEVER stores runtime values.
-- =====================================================

newtype Env a = Env (M.Map String a)
  deriving (Eq, Show)

-- Create an empty environment
makeEnv :: Env a
makeEnv = Env M.empty

-- Look up a name in the environment
lookupEnv :: String -> Env a -> Maybe a
lookupEnv x (Env m) = M.lookup x m

-- Extend the environment with a new binding
extendEnv :: String -> a -> Env a -> Env a
extendEnv x v (Env m) = Env (M.insert x v m)
