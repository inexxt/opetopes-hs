{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, StandaloneDeriving, DataKinds, FlexibleInstances, LambdaCase, ExistentialQuantification #-}


module Face where 

import Nattype
import qualified Opetope as O
import Subtype

em = O.OpetopeE -- how to import that as this name?

data ProdFace (dim :: Nat) where
    Point :: O.Opetope Z-> O.Opetope Z-> ProdFace Z
    -- TODO there should be refinment types - it has to have p1, p2 of dim > 1 
    Arrow :: String -> O.OpetopeE -> O.OpetopeE -> ProdFace Z -> ProdFace Z-> ProdFace (S Z)  
    Face :: String -> O.OpetopeE -> O.OpetopeE -> [ProdFace (S m)] -> ProdFace (S m) -> ProdFace (S (S m))


p1 :: ProdFace n -> O.OpetopeE
p1 (Point x _) = em x
p1 (Arrow _ x _ _ _) = x
p1 (Face _ x _ _ _) = x

p2 :: ProdFace n -> O.OpetopeE
p2 (Point _ y) = em y
p2 (Arrow _ _ y _ _) = y
p2 (Face _ _ y _ _) = y

-- TODO change that, it should be a very simple extension of O.dom and O.cod
dom :: ProdFace (S n) -> [ProdFace n]
dom (Arrow _ _ _ d _) = [d]
dom (Face _ _ _ d _) = d

cod :: ProdFace (S n) -> ProdFace n
cod (Arrow _ _ _ _ c) = c
cod (Face _ _ _ _ c) = c

deriving instance Eq (ProdFace dim)
-- deriving instance Ord (ProdFace dim) -- for some reason this doesn't work TODO

instance Ord (ProdFace dim) where
    (Point x1 y1) <= (Point x2 y2) = (x1, y1) <= (x2, y2)
    (Arrow a1 x1 y1 c1 d1) <= (Arrow a2 x2 y2 c2 d2) = (a1, x1, y1, c1, d1) <= (a2, x2, y2, c2, d2)
    (Face a1 x1 y1 c1 d1) <= (Face a2 x2 y2 c2 d2) = (a1, x1, y1, c1, d1) <= (a2, x2, y2, c2, d2)


-- deriving instance Show (ProdFace dim) -- TODO

instance Subtype (ProdFace dim) where 
    type SuperType (ProdFace dim) = O.Opetope dim
    embedImmediate (Point _ _) = O.Point ""
    embedImmediate (Face s _ _ d c) = O.Face s (map embedImmediate d) (embedImmediate c)

match :: ProdFace (S (S n)) -> Bool
match (Face _ _ _ d c) = O.match (map embedImmediate d) (embedImmediate c) -- TODO


data FaceE = forall n. FaceE (ProdFace n)

instance Eq FaceE where
    (FaceE (Point x1 y1)) == (FaceE (Point x2 y2)) = x1 == x2 && y1 == y2
    (FaceE (Arrow a1 x1 y1 _ _)) == (FaceE (Arrow a2 x2 y2 _ _)) = a1 == a2 && x1 == x2 && y1 == y2 -- Dlaczego to nie działa? Przecież powinno zejść rekurencyjnie ... && c == c' && d == d'
    (FaceE (Face a1 x1 y1 _ _)) == (FaceE (Face a2 x2 y2 _ _)) = a1 == a2 && x1 == x2 && y1 == y2 -- j.w. && c == c' && d == d'

    _ == _ = False

-- instance Ord FaceE where
--     (FaceE (Point a)) <= (FaceE (Point b)) = a <= b
--     (FaceE (Arrow a c d)) <= (FaceE (Arrow b c' d')) = a <= b -- j.w. (a, c, d) <= (b, c', d')
--     (FaceE (Face a c d)) <= (FaceE (Face b c' d')) = a <= b -- j.w. (a, c, d) <= (b, c', d')

--     (FaceE (Point _)) <= (FaceE (Arrow _ _ _)) = True
--     (FaceE (Point _)) <= (FaceE (Face _ _ _)) = True
--     (FaceE (Arrow _ _ _)) <= (FaceE (Face _ _ _)) = True


--     @staticmethod
--     def verify_construction(p1: Opetope, p2: Opetope, ins: 'Iterable[Face]' = (), out=None, name="") -> bool:
--         if not Opetope.match(ins, out, out.level + 1):
--             return False

--         face = Face(p1, p2, ins, out, name)

--         def get_pxs(f: 'Face', px) -> Opetope:
--             if not f.level:
--                 return Opetope(name=px(f).name)

--             out = get_pxs(f.out, px)
--             ins = [get_pxs(i, px) for i in f.ins if i.level == out.level]
--             return Opetope(ins=ins, out=out, name=px(f).name)  # (*)

--         op1 = get_pxs(face, lambda x: x.p1)
--         op2 = get_pxs(face, lambda x: x.p2)

--         # FIXME remove these
--         op1.name = "abecadło"  # I can trust in names of all things below me, but I can't in my name, as in (*)
--         op2.name = "abecadło"  # I can trust in names of all things below me, but I can't in my name, as in (*)

--         # We have to check here if this is a valid projection
--         # eg if all (recursivly) faces of self, projected on p1, together get us p1
--         # and similarly p2
--         if not (Opetope.is_valid_morphism(op1, p1) and Opetope.is_valid_morphism(op2, p2)):
--             return False

--         return True


from_arrow_and_point :: O.Opetope (S Z) -> O.Opetope Z -> ProdFace (S Z)
from_arrow_and_point arr pt = let (O.Arrow _ d c) = arr in
    Arrow "" (em arr) (em pt) (Point d pt) (Point c pt)

-- we can't just use from_arrow_and_point, because the order p1, p2 is important
from_point_and_arrow ::  O.Opetope Z -> O.Opetope (S Z) -> ProdFace (S Z)
from_point_and_arrow pt arr = let (O.Arrow _ d c) = arr in
    Arrow "" (em pt) (em arr) (Point pt d) (Point pt c)

from_arrow_and_arrow :: O.Opetope (S Z) -> O.Opetope (S Z) -> ProdFace (S Z)
from_arrow_and_arrow arr1 arr2 = 
    let (O.Arrow _ d1 c1) = arr1
        (O.Arrow _ d2 c2) = arr2 in 
            Arrow "" (em arr1) (em arr2) (Point d1 d2) (Point c1 c2)