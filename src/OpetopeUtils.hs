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

 
module OpetopeUtils where

import qualified Data.MultiSet as MS
import qualified Data.Set.Monad as S
import qualified Data.HashMap.Strict as M
import Unsafe.Coerce

import Nattype
import Opetope


data OpetopeE = forall n. OE (Opetope n)

instance Eq OpetopeE where
    (OE (Point a1)) == (OE (Point a2)) = a1 == a2
    (OE (Arrow a1 c1 d1)) == (OE (Arrow a2 c2 d2)) = a1 == a2 && c1 == c2 && d1 == d2 -- Dlaczego to nie działa? Przecież powinno zejść rekurencyjnie ... && c == c' && d == d'
    (OE (Face a c d)) == (OE (Face b c' d')) = a == b -- j.w. && c == c' && d == d'
    
    _ == _ = False

instance Ord OpetopeE where
    (OE (Point a)) <= (OE (Point b)) = a <= b
    (OE (Arrow a c d)) <= (OE (Arrow b c' d')) = a <= b -- j.w. (a, c, d) <= (b, c', d')
    (OE (Face a c d)) <= (OE (Face b c' d')) = a <= b -- j.w. (a, c, d) <= (b, c', d')

    (OE (Point _)) <= (OE (Arrow _ _ _)) = True
    (OE (Point _)) <= (OE (Face _ _ _)) = True
    (OE (Arrow _ _ _)) <= (OE (Face _ _ _)) = True



dim :: Opetope n -> SNat n
dim (Point _) = ZZ
dim (Arrow _ _ _) = SS ZZ
dim (Face _ _ c) = SS (dim c)

dim_n :: Opetope n -> Nat
dim_n = snat_to_nat . dim



-- Ugly hack for now
oe_to_opetope :: SNat n -> OpetopeE -> Opetope n
oe_to_opetope ZZ (OE (Point x)) = Point x
oe_to_opetope (SS ZZ) (OE (Arrow s x y)) = Arrow s x y
oe_to_opetope (SS (SS n)) (OE (Face s x y)) = undefined -- Face s x y
oe_to_opetope _ _ = undefined


-- this is this key moment - I want to have a function n:Nat -> Map n (Opetope n)
type OpetopesCollection = M.HashMap Nat (S.Set OpetopeE)

shmap :: Opetope n -> OpetopesCollection
shmap op = M.singleton (dim_n op) (S.singleton (OE op))

unions :: [OpetopesCollection] -> OpetopesCollection
unions [] = M.empty
unions (x:xs) = 
    let m = unions xs in
    M.unionWithKey (\_ v1 v2 -> v1 `S.union` v2) x m

subopetopes :: forall n. Opetope n -> OpetopesCollection
subopetopes op = case op of
    (Point x) -> shmap op
    (Arrow s d c) -> unions $ [shmap op, subopetopes d, subopetopes c]
    (Face s d c) -> unions $ [shmap op, (unions (map subopetopes d)), subopetopes c]

subouts :: forall n. Opetope n -> OpetopesCollection
subouts op = case op of
    (Point x) -> M.empty
    (Arrow s d c) -> shmap c
    (Face s d c) -> unions [shmap c, (unions (map subouts d)), subouts c]


is_valid_morphism :: Opetope n1 -> Opetope n2 -> Bool
is_valid_morphism op1 op2 = undefined

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
    (Face _ d c) -> (and (Prelude.map is_non_degenerated d)) && is_non_degenerated c


-- match :: Opetope Z -> Opetope Z -> Bool
-- match _ _ = True

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
