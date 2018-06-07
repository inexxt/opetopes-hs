{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, StandaloneDeriving, DataKinds, FlexibleInstances, LambdaCase, ExistentialQuantification #-}


module Face where 

import qualified Opetope as O
import Subtype


data ProdFace (dim :: O.Nat) where 
    Point :: String -> O.Opetope O.Z-> O.Opetope O.Z-> ProdFace O.Z
    Arrow :: String -> O.Opetope O.Z-> O.Opetope O.Z-> ProdFace O.Z-> ProdFace O.Z-> ProdFace (O.S O.Z)
    Face :: String -> O.Opetope n1 -> O.Opetope n2 -> [ProdFace (O.S m)] -> ProdFace (O.S m) -> ProdFace (O.S (O.S m))

deriving instance Eq (ProdFace dim)
deriving instance Show (ProdFace dim)

instance Subtype (ProdFace dim) where 
    type SuperType (ProdFace dim) = O.Opetope dim
    embedImmediate (Point s _ _) = O.Point s
    embedImmediate (Face s _ _ d c) = O.Face s (map embedImmediate d) (embedImmediate c)

match :: O.Opetope n1 -> O.Opetope n2 -> [ProdFace (O.S m)] -> ProdFace (O.S m) -> Bool
match _ _ d c = O.match (map embedImmediate d) (embedImmediate c)


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

--     @staticmethod
--     def from_point_and_point(p1: Opetope, p2: Opetope) -> 'Face':
--         assert (p1.level, p2.level) == (0, 0)
--         return Face(p1, p2)

--     @staticmethod
--     def from_arrow_and_point(p1: Opetope, p2: Opetope) -> 'Face':
--         assert (p1.level, p2.level) == (1, 0)
--         return Face(p1, p2, ins=[Face.from_point_and_point(p1.ins[0], p2)],
--                     out=Face.from_point_and_point(p1.out, p2))

--     @staticmethod
--     def from_point_and_arrow(p1: Opetope, p2: Opetope) -> 'Face':
--         assert (p1.level, p2.level) == (0, 1)
--         # we can't just use from_arrow_and_point, because the order p1, p2 is important
--         return Face(p1, p2, ins=[Face.from_point_and_point(p1, p2.ins[0])],
--                     out=Face.from_point_and_point(p1, p2.out))

--     @staticmethod
--     def from_arrow_and_arrow(p1: Opetope, p2: Opetope) -> 'Face':
--         assert (p1.level, p2.level) == (1, 1)
--         return Face(p1, p2, ins=[Face.from_point_and_point(p1.ins[0], p2.ins[0])],
--                     out=Face.from_point_and_point(p1.out, p2.out))

--     def __eq__(self, other):
--         return hash(self) == hash(other)

--     def __hash__(self):
--         return hash(self._str_full)

--     def __str__(self):
--         return self._str

--     def __repr__(self):
--         return self._str
