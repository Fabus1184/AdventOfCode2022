module LibTH (mktup, mktmap, mkuntup) where

import Control.Monad (replicateM)
import Data.Functor ((<&>))
import Language.Haskell.TH (Type, appE, appT, arrowT, clause, funD, listE, listP, listT, mkName, normalB, sigD, tupE, tupP, tupleT, varE, varP, varT)
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

funT :: [Q Type] -> Q Type
funT = foldr1 (appT . appT arrowT)

funT2 :: Q Type -> Q Type -> Q Type
funT2 a = appT (appT arrowT a)

tupleT' :: Int -> [Q Type] -> Q Type
tupleT' n = foldl appT (tupleT n)