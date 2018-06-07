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


deriving instance Eq (ProdFace dim)
-- deriving instance Show (ProdFace dim) -- TODO

instance Subtype (ProdFace dim) where 
    type SuperType (ProdFace dim) = O.Opetope dim
    embedImmediate (Point _ _) = O.Point ""
    embedImmediate (Face s _ _ d c) = O.Face s (map embedImmediate d) (embedImmediate c)

match :: O.OpetopeE -> O.Opetope n2 -> [ProdFace (S m)] -> ProdFace (S m) -> Bool
match _ _ d c = O.match (map embedImmediate d) (embedImmediate c) -- TODO


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