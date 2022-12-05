module LibTH (mktup, mktmap) where

import Data.Functor ((<&>))
import Language.Haskell.TH (Type, appE, appT, arrowT, clause, funD, listP, listT, mkName, normalB, sigD, tupE, tupP, tupleT, varE, varP, varT)
import Language.Haskell.TH.Syntax (Dec, Q, newName)

mktup :: Int -> Q [Dec]
mktup n = do
    let name = mkName $ "tup" ++ show n
    tvar <- newName "a" >>= varT
    vs <- mapM (const $ newName "x") [0 .. pred n]
    sequence
        [ sigD name (appT (appT arrowT (appT listT $ pure tvar)) $ (!! n) $ iterate (`appT` pure tvar) (tupleT n))
        , funD name [clause [listP $ map varP vs] (normalB $ tupE $ map varE vs) []]
        ]

mktmap :: Int -> Q [Dec]
mktmap n = do
    let name = mkName $ "tmap" ++ show n
    fs <- mapM (const $ newName "f") [0 .. pred n]
    vs <- mapM (const $ newName "x") [0 .. pred n]

    ts <- mapM (const (newName "a" <&> varT)) [0 .. pred n]
    ts' <- mapM (const (newName "a'" <&> varT)) [0 .. pred n]

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