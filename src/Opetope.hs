{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, StandaloneDeriving, DataKinds, FlexibleInstances, LambdaCase #-}

module Opetope
    ( Opetope, match
    ) where

import Text.Printf (printf)

data Nat = Z | S Nat



data Point = Point {nameP :: String} deriving (Eq)
instance Show Point where
    show (Point name) = show name

data Arrow = Arrow {nameA :: String, domA :: Opetope Z, codA :: Opetope Z} deriving (Eq)
instance Show Arrow where
    show (Arrow name dom cod)  = printf "(%s: %s -> %s)" (show name) (show dom) (show cod) 

data Face (dim :: Nat) where
    Face :: {nameF :: String, domF :: [Opetope dim], codF :: Opetope dim} -> Face (S dim)
instance Show (Face n) where
    show (Face name dom cod)  = printf "(%s: %s -> %s)" (show name) (show dom) (show cod) 

data Opetope (dim :: Nat) where
    OPoint :: Point -> Opetope Z
    OArrow :: Arrow -> Opetope (S Z)
    OFace :: Face dim -> Opetope (S dim)

instance Show (Opetope n) where
    show (OPoint x) = show x
    show (OArrow x) = show x
    show (OFace x) = show x

instance Eq (Opetope dim) where
    (OPoint x) == (OPoint y) = x == y
    (OArrow x) == (OArrow y) = x == y
    (OFace x) == (OFace y) = x == y

instance Ord (Opetope dim) where
    x <= y = (name x < name y) 
        where 
            name x = case x of
                OPoint t -> nameP t
                OArrow t -> nameA t
                OFace t -> nameF t

a = OPoint (Point "a")
b = OPoint (Point "b")
ab1 = OArrow (Arrow "ab1" a b)
ab2 = OArrow (Arrow "ab1" a b)
alpha = OFace (Face "alpha" [ab1] ab2)
(OFace alphaf) = alpha

match :: [Opetope dim] -> Opetope dim -> Bool
match ins out = isEmpty $ (map dom (map unpack ins)) ++ (dom out)
    where 
        dom x = case x of  
            OArrow _ -> domA
            _ -> domF
        unpack x = case x of
            OArrow t -> t
            OFace t -> t
            OPoint t -> t

someFunc :: IO ()
someFunc = putStrLn "someFunc"
