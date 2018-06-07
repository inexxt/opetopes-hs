{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TypeOperators,
             FlexibleInstances, FlexibleContexts, UndecidableInstances
  #-}

-- source: https://gist.github.com/harpocrates/38ec83098cd45d7e8bccbb2d7001acb5

-- Model subtyping when a given subtype has at most one immediate supertype (so you
-- have a tree of subtypes instead of a lattice.
module Subtype where

import Data.IORef (IORef)
-- | 'a' is an immediate subtype of 'b'. Formally,
--
--   * 'a <: b'
--   * 'a /= b'
--   * there does not exist a 'c' different from 'a' and 'b' such that 'a <: c <: b'
--
class Subtype a where
  type SuperType a :: *
  embedImmediate :: a -> SuperType a

-- | 'a' is a subtype of 'b'
class a <: b where
  -- | Embed a value of a subtype into a supertype
  embed :: a -> b

-- | Any type is a subtype of itself
instance a <: a where
  embed = id

-- | For 'a <: b', we get the immediate supertype 'c' of 'a' ('a <: c') and check that 'c <: b'
instance {-# OVERLAPPABLE #-} (Subtype a, (SuperType a) <: b) => a <: b where
    embed = embed . embedImmediate

instance {-# OVERLAPPING #-} (a <: b) => [a] <: [b] where
    embed = map embed

instance {-# OVERLAPPING #-} (a <: b, c <: d) => (b -> c) <: (a -> d) where
    embed f = embed . f . embed
  
instance {-# OVERLAPPING #-} (IORef a) <: (IORef a) where
    embed = id