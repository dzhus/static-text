{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sext.TH (mkSextable)

where

import           Prelude as P

import           Data.Proxy
import           Data.Sext.Class
import           Data.String

import           GHC.TypeLits

import           Language.Haskell.TH


mkSextable :: (Name -> Type)
           -- ^ Underlying type generator. It may be a unary type
           -- constructor, but you can also ignore the name produced.
           -> (Name -> Type)
           -- ^ Element type generator (for @[a]@, this is @a@). The
           -- name supplied is the same as in first argument.
           -> Name
           -- ^ Constructor name.
           -> Name
           -- ^ @append@ name.
           -> Name
           -- ^ @replicate@ name.
           -> Name
           -- ^ @map@ name.
           -> Name
           -- ^ @take@ name.
           -> Name
           -- ^ @drop@ name.
           -> DecsQ
mkSextable type' elem' con' append' replicate' map' take' drop' =
  do
    n <- newName "a"
    -- Associated types:
    let instances =
          [ TySynInstD ''Elem (TySynEqn [type' n] (elem' n))
          , DataInstD [] ''Sext [SigT (VarT $ mkName "i") (ConT ''Nat)
                              , type' n]
            [NormalC con' [(NotStrict, type' n)]]
            []
          ]

    -- Class members
    let d1 = FunD (mkName "unsafeCreate")
             [Clause [] (NormalB $ ConE con') []]

    n2 <- newName "s"
    let d2 = FunD (mkName "unwrap")
             [Clause [ConP con' [VarP n2]] (NormalB $ VarE n2) []]

    let f2app f a b =
          NormalB $ AppE (ConE con') (AppE (AppE (VarE f) (VarE a)) (VarE b))

    n31 <- newName "a"
    n32 <- newName "b"
    let d3 = FunD (mkName "append")
             [Clause [ConP con' [VarP n31], ConP con' [VarP n32]]
              (f2app append' n31 n32) []
             ]

    n41 <- newName "f"
    n42 <- newName "s"
    let d4 = FunD (mkName "map")
             [Clause [VarP n41, ConP con' [VarP n42]]
              (f2app map' n41 n42) []
             ]

    -- Produce and expression to convert a type-level natural to Int
    let tLen lenName =
          AppE
          (VarE 'P.fromIntegral)
          (AppE
           (VarE 'natVal)
           (SigE
            (ConE 'Proxy)
            (AppT (ConT ''Proxy) (VarT lenName))))

    let cutFun method funName = do
         n51 <- newName "s"
         n52 <- newName "m"
         n53 <- newName "n"
         return $
           [ SigD (mkName method) $
             ForallT [PlainTV n52, PlainTV n53]
             [ ClassP ''KnownNat [VarT n52]
             , ClassP ''KnownNat [VarT n53]
             , EqualP (ConT 'True)
               (AppT (AppT (ConT ''(<=?)) (VarT n53)) (VarT n52))
             ] $
             AppT (AppT ArrowT (AppT (AppT (ConT ''Sext) (VarT n52)) (type' n)))
             (AppT (AppT (ConT ''Sext) (VarT n53)) (type' n))
           , FunD (mkName method)
             [ Clause
               [(ConP con' [VarP n51])]
               (NormalB $
                AppE (ConE con') $
                AppE (AppE (VarE funName) (tLen n53))
                (VarE n51)) []
             ]
           ]

    d5 <- cutFun "take" take'
    d6 <- cutFun "drop" drop'

    n71 <- newName "c"
    n72 <- newName "m"
    let d7 =
           [ SigD (mkName "replicate") $
             ForallT [PlainTV n72]
             [ ClassP ''KnownNat [VarT n72]
             ] $
             AppT (AppT ArrowT (AppT (ConT ''Elem) (type' n)))
             (AppT (AppT (ConT ''Sext) (VarT n72)) (type' n))
           , FunD (mkName "replicate")
             [ Clause
               [VarP n71]
               (NormalB $
                AppE (ConE con') $
                AppE (AppE (VarE replicate') (tLen n72))
                (VarE n71)) []
             ]
           ]

    let decs =  instances ++ [d1, d2, d3, d4] ++ d5 ++ d6 ++ d7
    return [InstanceD [] (AppT (ConT ''Sextable) (type' n)) decs]
