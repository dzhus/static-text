{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Template Haskell helpers for StaticText.

-}

module Data.StaticText.TH
       ( st
       )

where

import           Prelude
import qualified Prelude as P (length)

import           Data.StaticText.Class
import           Data.String

import           Language.Haskell.TH


-- | A type with IsString instance to allow string literals in 'st'
-- argument without quoting.
newtype LitS = LitS String deriving IsString


-- | Type-safe StaticText constructor macro for string literals.
--
-- Example:
--
-- > $(st "Foobar")
--
-- compiles to
--
-- > unsafeCreate "Foobar" :: forall a. (IsString a, IsStaticText a) => StaticText 6 a
--
-- where 6 is the string length obtained at compile time.
st :: LitS -> Q Exp
st (LitS s) =
  do
    at <- newName "a"
    return $ SigE (AppE (VarE 'unsafeCreate) (LitE $ StringL s))
                (ForallT
                 [PlainTV at]
#if MIN_VERSION_template_haskell(2,10,0)
                 [ AppT (ConT ''IsString) (VarT at)
                 , AppT (ConT ''IsStaticText) (VarT at)] $
#else
                 [ ClassP ''IsString [VarT at]
                 , ClassP ''IsStaticText [VarT at]] $
#endif
                 AppT
                 (AppT
                  (ConT ''Static)
                  (VarT at))
                 (LitT $ NumTyLit (fromIntegral $ P.length s)))
