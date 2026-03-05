module Env
  ( Env(..)
  , makeEnv
  , lookupEnv
  , extendEnv
  ) where

-- =====================================================
-- ENVIRONMENT (SYMBOL TABLE)
-- =====================================================
-- This module is used by COMPILER PASSES.
-- It maps names to metadata (usually other names).
-- It NEVER stores runtime values.
-- =====================================================

newtype Env a = Env [(String, a)]
  deriving (Eq, Show)

-- Create an empty environment
makeEnv :: Env a
makeEnv = Env []

-- Look up a name in the environment
lookupEnv :: String -> Env a -> Maybe a
lookupEnv x (Env pairs) = lookup x pairs

-- Extend the environment with a new binding
extendEnv :: String -> a -> Env a -> Env a
extendEnv x v (Env pairs) = Env ((x, v) : pairs)