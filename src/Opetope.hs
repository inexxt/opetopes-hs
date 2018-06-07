{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, StandaloneDeriving, DataKinds, FlexibleInstances, LambdaCase, ExistentialQuantification #-}

 
module Opetope where

import Text.Printf (printf)
import qualified Data.MultiSet as S
import Data.List.Unique (unique)


data Nat = Z | S Nat deriving (Eq, Ord, Show)


data Opetope (dim :: Nat) where
    Point :: String -> Opetope Z
    Arrow :: String -> Opetope Z -> Opetope Z -> Opetope (S Z)
    -- this should in theory be  S n -> S n -> S S n, but it makes a problem in Faces
    -- so let's keep it like that for the time being
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
match ins out = not $ null $ (all_dom `S.union` out_cod) `S.difference` (all_cod `S.union` out_cod)
    where
        all_dom = (S.fromList (concat $ map dom ins))
        out_dom =  (dom out) `S.insert` S.empty
        all_cod = (S.fromList (map cod ins))
        out_cod =  (cod out) `S.insert` S.empty


is_unary :: Opetope (S dim) -> Bool
is_unary op = (length (dom op)) == 1

data OpetopeE = forall n. OpetopeE (Opetope n)


instance Eq OpetopeE where
    (OpetopeE (Point a)) == (OpetopeE (Point b)) = a == b
    (OpetopeE (Arrow a c d)) == (OpetopeE (Arrow b c' d')) = a == b -- Dlaczego to nie działa? Przecież powinno zejść rekurencyjnie ... && c == c' && d == d'
    (OpetopeE (Face a c d)) == (OpetopeE (Face b c' d')) = a == b -- j.w. && c == c' && d == d'

    (OpetopeE (Point _)) == (OpetopeE (Arrow _ _ _)) = False
    (OpetopeE (Point _)) == (OpetopeE (Face _ _ _)) = False
    (OpetopeE (Arrow _ _ _)) == (OpetopeE (Face _ _ _)) = False


instance Ord OpetopeE where
    (OpetopeE (Point a)) <= (OpetopeE (Point b)) = a <= b
    (OpetopeE (Arrow a c d)) <= (OpetopeE (Arrow b c' d')) = a <= b -- j.w. (a, c, d) <= (b, c', d')
    (OpetopeE (Face a c d)) <= (OpetopeE (Face b c' d')) = a <= b -- j.w. (a, c, d) <= (b, c', d')

    (OpetopeE (Point _)) <= (OpetopeE (Arrow _ _ _)) = True
    (OpetopeE (Point _)) <= (OpetopeE (Face _ _ _)) = True
    (OpetopeE (Arrow _ _ _)) <= (OpetopeE (Face _ _ _)) = True

subopetopes :: forall n. Opetope n -> [OpetopeE]
subopetopes op = case op of
    (Point x) -> [OpetopeE op]
    (Arrow s d c) -> concat $ unique $ [[OpetopeE op], subopetopes d, subopetopes c]
    (Face s d c) -> concat $ unique $ [[OpetopeE op], (concat (map subopetopes d)), subopetopes c]

subouts :: forall n. Opetope n -> [OpetopeE]
subouts op = case op of
    (Point x) -> []
    (Arrow s d c) -> [OpetopeE c]
    (Face s d c) -> concat $ unique $ [[OpetopeE c], (concat (map subouts d)), subouts c]


is_valid_morphism :: Opetope n1 -> Opetope n2 -> Bool
is_valid_morphism op1 op2 = True

--     @staticmethod
--     def is_valid_morphism(op1: 'Opetope', op2: 'Opetope') -> bool:
--         """
--         Check that op1, with vertices colored (named) by vertices of op2, is a valid contraction to op2
--         One problem is that we can't use the top-level name
--         :param op1:
--         :param op2:
--         :return:
--         """

--         # contract all things in op1
--         def contract(op):
--             if not op.level:
--                 return op
--             out = contract(op.out)
--             ins = [contract(i) for i in op.ins]
--             ins = [i for i in ins if i.level == out.level]

--             if all([i._str == out._str for i in ins]):
--                 return contract(op.out)
--             return Opetope(ins=ins, out=out, name=op.name)

--         op1.name = op2.name  # FIXME ALARM ugly hack because of "abecadło" problem
--         return contract(op1)._str == op2._str


is_non_degenerated :: Opetope n -> Bool
is_non_degenerated op = case op of
    (Point s) -> True
    (Arrow _ d c) -> c /= d
    (Face _ d c) -> (and (map is_non_degenerated d)) && is_non_degenerated c


-- match :: Opetope Z -> Opetope Z -> Bool
-- match _ _ = True

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
