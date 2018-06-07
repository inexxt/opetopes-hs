{-# LANGUAGE TypeFamilies, GADTs, KindSignatures, StandaloneDeriving, DataKinds, FlexibleInstances, LambdaCase, ExistentialQuantification, MonadComprehensions #-}

module Product where

import Opetope as O
import Face as F
import Data.Set.Monad as S
import Nattype

data OpetopeOrder n where
    OpetopeOrder :: {op :: Opetope n, ord :: String} -> OpetopeOrder n-- TODO undefined

-- 
build_possible_faces :: F.ProdFace (S n) -> [F.ProdFace (S n)] -> OpetopeOrder p -> OpetopeOrder q -> S.Set (ProdFace (S (S n)))
build_possible_faces considered_codomain building_blocks orderP orderQ = dfs (S.fromList [F.cod considered_codomain]) S.empty (S.fromList building_blocks) considered_codomain orderP orderQ

dfs :: S.Set (F.ProdFace n) -> S.Set (F.ProdFace (S n)) -> S.Set (F.ProdFace (S n)) -> F.ProdFace (S n) -> OpetopeOrder p -> OpetopeOrder q -> S.Set(ProdFace (S (S n)))
dfs ins used building_blocks target_out orderP orderQ = let f = F.Face "" (F.em (op orderP)) (F.em (op orderQ)) (S.toList used) target_out in 
    if F.match f 
        then S.singleton f
        else do
            b <- (building_blocks S.\\ used)
            i <- ins
            return undefined