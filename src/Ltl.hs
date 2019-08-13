module Ltl
  ( Ltl(..)
  ) where

data Ltl
  = Var Int
  | SVar String
  | LTrue
  | LFalse
  | And [Ltl]
  | Or [Ltl]
  | Not Ltl
  | Next Ltl
  | Until Ltl Ltl
  | WeakUntil Ltl Ltl
  | Globally Ltl
  | Finally Ltl
  | Release Ltl Ltl
  deriving (Show)
