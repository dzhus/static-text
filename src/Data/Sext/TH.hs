{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sext.TH
       ( mkSextable
       , sext
       )

where

import           Prelude (($), (==), (-), (++), Bool(..), Maybe(..), return)
import qualified Prelude as P (fromIntegral, length)

import           Data.Proxy
import           Data.Sext.Class
import           Data.String

import           GHC.TypeLits

import           Language.Haskell.TH


mkSextable :: (Name -> Type)
           -- ^ Underlying type generator. It may be a unary type
           -- constructor, but you may also ignore the name produced.
           -> (Name -> Type)
           -- ^ Element type generator (for @[a]@, this is @a@). The
           -- name supplied is the same as in first argument.
           -> Name
           -- ^ Constructor name.
           -> Name
           -- ^ @length@ name. Used only for run-time length checks in
           -- constructor methods.
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
mkSextable type' elem' con' length' append' replicate' map' take' drop' =
  do
    -- Type variable possibly used by type constructor (type')
    n <- newName "a"

    -- Associated types
    t0 <- newName "i"
    let instances =
          [ TySynInstD ''Elem (TySynEqn [type' n] (elem' n))
          , DataInstD [] ''Sext [SigT (VarT t0) (ConT ''Nat)
                              , type' n]
            [NormalC con' [(NotStrict, type' n)]]
            []
          ]

    -- Produce an expression to convert a type-level natural to Int
    let tLen lenName =
          AppE
          (VarE 'P.fromIntegral)
          (AppE
           (VarE 'natVal)
           (SigE
            (ConE 'Proxy)
            (AppT (ConT ''Proxy) (VarT lenName))))

    -- unsafeCreate
    let uCreateD =
          [FunD 'unsafeCreate [Clause [] (NormalB $ ConE con') []]]

    -- create
    t1 <- newName "i"
    a1 <- newName "s"
    e1 <- [e|
           if $(varE length') $(varE a1) == $(return $ tLen t1)
           then Just ($(conE con') $(varE a1))
           else Nothing
          |]
    let createD =
          [ SigD 'create $
            ForallT [KindedTV t1 (ConT ''Nat)]
            [ClassP ''KnownNat [VarT t1]] $
            (AppT (AppT ArrowT (type' n))
              (AppT (ConT ''Maybe)
                    (AppT (AppT (ConT ''Sext) (VarT t1)) (type' n))))
          , FunD 'create
            [ Clause
              [VarP a1]
              (NormalB e1)
              []
            ]
          ]

    -- unwrap
    a2 <- newName "s"
    let unwrapD =
          FunD 'unwrap
          [Clause [ConP con' [VarP a2]] (NormalB $ VarE a2) []]

    -- 2-arg function application wrapped in the constructor
    let f2app f a b =
          NormalB $ AppE (ConE con') (AppE (AppE (VarE f) (VarE a)) (VarE b))

    -- append
    a31 <- newName "a"
    a32 <- newName "b"
    let appendD =
          FunD 'append
          [ Clause
            [ConP con' [VarP a31], ConP con' [VarP a32]]
            (f2app append' a31 a32)
            []
          ]

    -- map
    a41 <- newName "f"
    a42 <- newName "s"
    let mapD =
          FunD 'map
          [ Clause
            [VarP a41, ConP con' [VarP a42]]
            (f2app map' a41 a42)
            []
          ]

    let cutFun method funName = do
         n51 <- newName "s"
         n52 <- newName "m"
         n53 <- newName "n"
         return $
           [ SigD method $
             ForallT [PlainTV n52, PlainTV n53]
             [ ClassP ''KnownNat [VarT n52]
             , ClassP ''KnownNat [VarT n53]
             , EqualP (ConT 'True)
               (AppT (AppT (ConT ''(<=?)) (VarT n53)) (VarT n52))
             ] $
             AppT (AppT ArrowT (AppT (AppT (ConT ''Sext) (VarT n52)) (type' n)))
             (AppT (AppT (ConT ''Sext) (VarT n53)) (type' n))
           , FunD method
             [ Clause
               [(ConP con' [VarP n51])]
               (NormalB $
                AppE (ConE con') $
                AppE (AppE (VarE funName) (tLen n53))
                (VarE n51)) []
             ]
           ]

    takeD <- cutFun 'take take'
    dropD <- cutFun 'drop drop'

    n71 <- newName "c"
    n72 <- newName "m"
    e7 <- [e|
           unsafeCreate $
           $(varE replicate') $(return $ tLen n72) $(varE n71)
          |]
    let replicateD =
           [ SigD (mkName "replicate") $
             ForallT [PlainTV n72]
             [ ClassP ''KnownNat [VarT n72]
             ] $
             AppT (AppT ArrowT (AppT (ConT ''Elem) (type' n)))
             (AppT (AppT (ConT ''Sext) (VarT n72)) (type' n))
           , FunD (mkName "replicate")
             [ Clause
               [VarP n71]
               (NormalB e7)
               []
             ]
           ]

    -- createX signature
    let crSig fName sizeName =
          SigD fName $
          ForallT [KindedTV sizeName (ConT ''Nat)]
          [ClassP ''KnownNat [VarT sizeName]] $
          AppT
          (AppT ArrowT (AppT (ConT ''Elem) (type' n)))
          (AppT (AppT ArrowT (type' n))
           (AppT
            (AppT (ConT ''Sext) (VarT sizeName)) (type' n)))

    -- createLeft
    t8 <- newName "i"
    a81 <- newName "e"
    a82 <- newName "s"
    e8 <- [e|
           let
             t = P.fromIntegral $ natVal (Proxy :: Proxy $(varT t8))
             l = $(varE length') $(varE a82)
           in
             unsafeCreate $
             $(varE take') t $
             $(varE append') $(varE a82) $
             $(varE replicate') (t - l) $(varE a81)
           |]
    let crLeftD =
          [ crSig 'createLeft t8
          , FunD 'createLeft
            [ Clause
              [VarP a81, VarP a82]
              (NormalB $ e8)
              []
            ]
          ]

    -- createRight
    t9 <- newName "i"
    a91 <- newName "e"
    a92 <- newName "s"
    e9 <- [e|
           let
             t = P.fromIntegral $ natVal (Proxy :: Proxy $(varT t9))
             l = $(varE length') $(varE a92)
           in
             unsafeCreate $
             $(varE drop') (l - t) $
             $(varE append')
             ($(varE replicate') (t - l) $(varE a91))
             $(varE a92)
           |]
    let crRightD =
          [ crSig 'createRight t9
          , FunD 'createRight
            [ Clause
              [VarP a91, VarP a92]
              (NormalB $ e9)
              []
            ]
          ]

    let decs = instances ++
               crLeftD ++
               crRightD ++
               createD ++
               uCreateD ++
               [unwrapD, appendD, mapD] ++
               takeD ++
               dropD ++
               replicateD
    return [InstanceD [] (AppT (ConT ''Sextable) (type' n)) decs]


-- | A type with IsString instance to allow string literals in 'sext'
-- argument without quoting.
newtype LitS = LitS String deriving IsString


-- | Type-safe Sext constructor macro for string literals.
--
-- Example:
--
-- > $(sext "Foobar")
--
-- compiles to
--
-- > unsafeCreate "Foobar" :: forall a. (IsString a, Sextable a) => Sext 6 a
sext :: LitS -> Q Exp
sext (LitS s) =
  do
    at <- newName "a"
    return $ SigE (AppE (VarE 'unsafeCreate) (LitE $ StringL s))
                (ForallT
                 [PlainTV at]
                 [ ClassP ''IsString [VarT at]
                 , ClassP ''Sextable [VarT at]] $
                 (AppT
                  (AppT
                   (ConT ''Sext)
                   (LitT $ NumTyLit (P.fromIntegral $ P.length s)))
                  (VarT at)))
