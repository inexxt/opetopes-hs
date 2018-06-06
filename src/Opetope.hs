{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, StandaloneDeriving, DataKinds, FlexibleInstances, LambdaCase #-}

 
module Opetope where

import Text.Printf (printf)
import qualified Data.MultiSet as S

data Nat = Z | S Nat deriving (Eq, Ord, Show)


data Opetope (dim :: Nat) where
    Point :: String -> Opetope Z
    Arrow :: String -> Opetope Z -> Opetope Z -> Opetope (S Z)
    Face :: String -> [Opetope n] -> Opetope n -> Opetope (S n)
    

deriving instance Eq (Opetope a)
deriving instance Show (Opetope a)
--  how to add dimension comparing
instance Ord (Opetope dim) where
    (Point a) <= (Point b) = a <= b
    (Arrow a c d) <= (Arrow b c' d') = a <= b || c <= c' || d <= d'
    (Face a c d) <= (Face b c' d') = a <= b || c <= c' || d <= d'

dom :: Opetope (S n) -> [Opetope n]
dom (Arrow _ d _) = [d]
dom (Face _ d _) = d

cod :: Opetope (S n) -> Opetope n
cod (Arrow _ _ c) = c
cod (Face _ _ c) = c


match :: [Opetope (S dim)] -> Opetope (S dim) -> Bool
match ins out = not $ null $ (all_dom `S.union` out_cod) `S.difference` (all_cod `S.union` out_cod)
    where
        all_dom = (S.fromList (concat $ map dom ins))
        out_dom =  (dom out) `S.insert` S.empty
        all_cod = (S.fromList (map cod ins))
        out_cod =  (cod out) `S.insert` S.empty

-- match :: Opetope Z -> Opetope Z -> Bool
-- match _ _ = True

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
