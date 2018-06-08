{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, StandaloneDeriving, DataKinds, FlexibleInstances, LambdaCase, ExistentialQuantification, MonadComprehensions #-}

module Product where

import qualified Opetope as O
import qualified OpetopeUtils as U

import Face as F
import Data.Set.Monad as S
import Nattype

data OpetopeOrder n where
    OpetopeOrder :: {op :: O.Opetope n, ord :: S.Set (U.OpetopeE -> U.OpetopeE) } -> OpetopeOrder n-- TODO undefined

--
build_possible_faces :: F.ProdFace (S n) -> [F.ProdFace (S n)] -> OpetopeOrder pn -> OpetopeOrder qn -> S.Set (ProdFace (S (S n)))
build_possible_faces considered_codomain building_blocks orderP orderQ = 
    dfs (S.fromList [F.cod considered_codomain]) S.empty (S.fromList building_blocks) considered_codomain orderP orderQ

dfs :: S.Set (F.ProdFace n) -> S.Set (F.ProdFace (S n)) -> S.Set (F.ProdFace (S n)) -> F.ProdFace (S n) -> OpetopeOrder pn -> OpetopeOrder qn -> S.Set(ProdFace (S (S n)))
dfs ins used building_blocks target_out (OpetopeOrder {op=p, ord=ordP}) (OpetopeOrder {op=q, ord=ordQ}) = 
    let f = F.Face "" (F.em q) (F.em q) (S.toList used) target_out 
        orderP = OpetopeOrder {op=p, ord=ordP}
        orderQ = OpetopeOrder {op=q, ord=ordQ} in
    if F.match f 
        then S.singleton f
        else S.unions [(dfs new_ins new_used building_blocks target_out orderP orderQ) | b <- S.toList (building_blocks S.\\ used), 
                                                                                         i <- S.toList ins, 
                                                                                         let new_ins = ins `S.union` (S.fromList (F.dom b)),
                                                                                         let new_used = b `S.insert` used,
                                                                                         matching_cond i b,
                                                                                         order_cond b]
            where 
                matching_cond i b = undefined -- (i == (F.cod b)) && (p1 i) `S.member` (U.subopetopes p) && (p2 i) `S.member` (U.subopetopes q)
                order_cond b = True -- (and [or [ordP (F.em bi) (F.em ti) | bi <- (O.dom (p1 b))] | ti <- (O.dom (p1 t))]) && (and [or [ordQ (F.em bi) (F.em ti) | bi <- (O.dom (p2 b))] | ti <- (O.dom (p2 t))])
                    -- ((b.p1.level < target_out.p1.level)
                    -- (b.p2.level < target_out.p2.level)

product :: OpetopeOrder pn -> OpetopeOrder qn -> (S.Set FaceE, S.Set FaceE)
product orderP orderQ = 
    let p = op orderP
        q = op orderQ
        subsp = U.subopetopes p
        subsq = U.subopetopes q
        points s = undefined in undefined