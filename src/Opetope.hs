{-# LANGUAGE TypeFamilies, 
             GADTs, 
             KindSignatures, 
             StandaloneDeriving, 
             DataKinds, 
             FlexibleInstances, 
             LambdaCase, 
             ExistentialQuantification,
             DeriveDataTypeable,
             TemplateHaskell
            #-}

 
module Opetope where

import Text.Printf (printf)
import qualified Data.MultiSet as MS
import Data.Set.Monad as S

import Data.List.Unique (unique)
import Data.Data
import Data.HashMap.Strict as M
import Nattype

data Opetope (dim :: Nat) where
    Point :: String -> Opetope Z
    Arrow :: String -> Opetope Z -> Opetope Z -> Opetope (S Z)
    Face :: String -> [Opetope (S n)] -> Opetope (S n) -> Opetope (S (S n))


deriving instance Eq (Opetope a)
deriving instance Show (Opetope a)

--  how to add dimension comparing
instance Ord (Opetope dim) where
    (Point a) <= (Point b) = a <= b
    (Arrow a c d) <= (Arrow b c' d') = (a, c, d) <= (b, c', d')
    (Face a c d) <= (Face b c' d') = (a, c, d) <= (b, c', d')

dom :: Opetope (S n) -> [Opetope n]
dom (Arrow _ d _) = [d]
dom (Face _ d _) = d

cod :: Opetope (S n) -> Opetope n
cod (Arrow _ _ c) = c
cod (Face _ _ c) = c

match :: [Opetope (S dim)] -> Opetope (S dim) -> Bool
match ins out = not $ Prelude.null $ (all_dom `MS.union` out_cod) `MS.difference` (all_cod `MS.union` out_cod)
    where
        all_dom = (MS.fromList (concat $ Prelude.map dom ins))
        out_dom =  MS.singleton (dom out)
        all_cod = (MS.fromList (Prelude.map cod ins))
        out_cod =  MS.singleton (cod out)


is_unary :: Opetope (S dim) -> Bool
is_unary op = (length (dom op)) == 1