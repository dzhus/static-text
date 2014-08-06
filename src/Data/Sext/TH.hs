{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Template Haskell helpers for Sext.

-}

module Data.Sext.TH
       ( sext
       , mkSextable
       )

where

import           Prelude (($), (==), (-), (++), Bool(..), Maybe(..), return)
import qualified Prelude as P (fromIntegral, length)

import           Data.Proxy
import           Data.Sext.Class
import           Data.String

import           GHC.TypeLits

import           Language.Haskell.TH


-- | Generate 'Sextable' instance for a type (@s@) using the provided
-- names of basic operations for the type. Types of the provided
-- functions must follow conventions set by standard Haskell list
-- functions.
--
-- This macro is used to produce all bundled instances of Sextable.
-- For example, an instance for lists is generated using standard
-- Prelude functions as follows:
--
-- > mkSextable
-- > (AppT ListT . VarT)
-- > (VarT)
-- > (mkName "List")
-- > 'length
-- > '(++)
-- > 'replicate
-- > 'map
-- > 'take
-- > 'drop
mkSextable :: (Name -> Type)
           -- ^ Underlying type generator. It may be a unary type
           -- constructor, but you may also ignore the name produced.
           -> (Name -> Type)
           -- ^ Element type generator (for @[a]@, this is @a@). The
           -- name supplied is the same as in the first argument.
           -> Name
           -- ^ Constructor name for 'Sext' data family.
           -> Name
           -- ^ @length :: s -> Int@. Used only for run-time length
           -- checks in constructor methods.
           -> Name
           -- ^ @append :: s -> s -> s@.
           -> Name
           -- ^ @replicate :: Int -> Elem s -> s@.
           -> Name
           -- ^ @map :: (Elem a -> Elem a) -> s -> s@.
           -> Name
           -- ^ @take :: Int -> s -> s@.
           -> Name
           -- ^ @drop :: Int -> s -> s@.
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

    let cutSig fName fromSize toSize=
          SigD fName $
          ForallT [PlainTV fromSize, PlainTV toSize]
          [ ClassP ''KnownNat [VarT fromSize]
          , ClassP ''KnownNat [VarT toSize]
          , EqualP (ConT 'True)
            (AppT (AppT (ConT ''(<=?)) (VarT toSize)) (VarT fromSize))
          ] $
          AppT (AppT ArrowT
                (AppT (AppT (ConT ''Sext) (VarT fromSize)) (type' n)))
          (AppT (AppT (ConT ''Sext) (VarT toSize)) (type' n))

    -- take
    a51 <- newName "s"
    t51 <- newName "m"
    t52 <- newName "n"
    e5 <- [e|
           $(conE con') $
           $(varE take') $(return $ tLen t52) $(varE a51)
          |]
    let takeD =
          [ cutSig 'take t51 t52
          , FunD 'take
            [ Clause
              [(ConP con' [VarP a51])]
              (NormalB e5)
              []
            ]
          ]

    -- drop
    a51' <- newName "s"
    t51' <- newName "m"
    t52' <- newName "n"
    e5' <- [e|
           $(conE con') $
           $(varE drop')
           ($(varE length') $(varE a51') - $(return $ tLen t52'))
           $(varE a51')
          |]
    let dropD =
          [ cutSig 'drop t51' t52'
          , FunD 'drop
            [ Clause
              [(ConP con' [VarP a51'])]
              (NormalB e5')
              []
            ]
          ]

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
--
-- where 6 is the string length obtained at compile time.
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
