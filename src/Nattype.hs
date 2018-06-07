module Nattype where

data Nat = Z | S Nat deriving (Eq, Ord, Show)
