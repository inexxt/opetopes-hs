{-# LANGUAGE 
    DefaultSignatures,
    EmptyCase,
    ExistentialQuantification,
    FlexibleContexts,
    FlexibleInstances,
    GADTs,
    InstanceSigs,
    KindSignatures,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances
    #-}

module Nattype where
import Data.Singletons as S

import Data.Singletons.TH as STH
import Data.Hashable

-- Doesnt work for some reason
STH.singletons [d|
    data Nat = Z | S Nat deriving (Eq, Ord, Show)
    |]

-- data Nat = Z | S Nat deriving (Eq, Ord, Show)

-- instance Hashable Nat where
--     hashWithSalt s Z = s
--     hashWithSalt s (S n) = s + (hash n) + 1

-- data SNat (n :: Nat) where
--     ZZ :: SNat Z
--     SS :: SNat n -> SNat (S n)

-- snat_to_nat :: SNat n -> Nat
-- snat_to_nat ZZ = Z
-- snat_to_nat (SS n) = S (snat_to_nat n)
