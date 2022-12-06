{-# LANGUAGE TemplateHaskellQuotes #-}

module LibTH (mktup, mktmap, mkuntup, mkttake) where

import Control.Lens ((^.))
import Control.Monad (replicateM)
import Data.Functor (($>), (<&>))
import Data.List.Index (imapM)
import Language.Haskell.TH (Name, Type, appE, appT, arrowT, clause, conT, forallT, funD, listE, listP, listT, mkName, normalB, plainTV, sigD, specifiedSpec, tupE, tupP, tupleT, varE, varP, varT)
import Language.Haskell.TH.Syntax (Dec, Q, newName)

mktup :: Int -> Q [Dec]
mktup n = do
    let name = mkName $ "tup" ++ show n
    tvar <- newName "a" >>= varT
    vs <- replicateM n (newName "x")
    sequence
        [ sigD name $ funT2 (appT listT $ pure tvar) (iterate (`appT` pure tvar) (tupleT n) !! n)
        , funD name [clause [listP $ map varP vs] (normalB $ tupE $ map varE vs) []]
        ]

mkuntup :: Int -> Q [Dec]
mkuntup n = do
    let name = mkName $ "untup" ++ show n
    tvar <- newName "a" >>= varT
    vs <- replicateM n (newName "x")
    sequence
        [ sigD name $ funT2 (iterate (`appT` pure tvar) (tupleT n) !! n) (listT `appT` pure tvar)
        , funD name [clause [tupP $ map varP vs] (normalB $ listE $ map varE vs) []]
        ]

mktmap :: Int -> Q [Dec]
mktmap n = do
    let name = mkName $ "tmap" ++ show n
    fs <- replicateM n (newName "f")
    vs <- replicateM n (newName "x")
    ts <- replicateM n (newName "a" <&> varT)
    ts' <- replicateM n (newName "a'" <&> varT)
    sequence
        [ sigD name $ funT [tupleT' n (zipWith funT2 ts ts'), tupleT' n ts, tupleT' n ts']
        , funD name [clause [tupP $ map varP fs, tupP $ map varP vs] (normalB $ tupE $ zipWith appE (map varE fs) (map varE vs)) []]
        ]

mkttake :: Int -> Q [Dec]
mkttake n = do
    let name = mkName $ "ttake" ++ show n
    x <- newName "x"
    ts <- replicateM n (newName "a")
    s <- newName "s"
    let l = map (($> specifiedSpec) . plainTV) $ s : ts
    let cxt = imapM (\i t -> fieldT (succ i) (s, s, t, t)) ts
    sequence
        [ sigD name $ forallT l cxt $ funT2 (varT s) (tupleT' n $ map varT ts)
        , funD name [clause [varP x] (normalB $ tupE $ take n $ map (\i -> varE '(^.) `appE` varE x `appE` varE (mkName $ "_" <> show i)) [1 .. n]) []]
        ]

fieldT :: Int -> (Name, Name, Name, Name) -> Q Type
fieldT i (a, b, c, d) = appT (appT (appT (appT (conT $ mkName $ "Field" <> show i) (varT a)) (varT b)) (varT c)) (varT d)

funT :: [Q Type] -> Q Type
funT = foldr1 (appT . appT arrowT)

funT2 :: Q Type -> Q Type -> Q Type
funT2 = appT . appT arrowT

tupleT' :: Int -> [Q Type] -> Q Type
tupleT' n = foldl appT (tupleT n)