{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Sext.Class
       ( Sextable(..)
       )

where

import qualified Prelude as P

import           Data.Proxy

import           GHC.TypeLits


class Sextable a where
  type Elem a
  data Sext (i :: Nat) a

  create :: (KnownNat i) => a -> P.Maybe (Sext i a)
  create' :: (KnownNat i) => a -> Sext i a
  unsafeCreate :: a -> Sext i a
  unwrap :: Sext i a -> a

  append :: Sext m a -> Sext n a -> Sext (m + n) a
  replicate :: KnownNat m => Elem a -> Sext m a
  map :: (Elem a -> Elem a) -> Sext m a -> Sext m a

  take :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => Sext m a -> Sext n a
  drop :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => Sext m a -> Sext n a
