{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Sext.Class
       ( Sextable(..)
       , length
       , padLeft
       , padRight
       )

where

import qualified Prelude as P

import           Data.Proxy

import           GHC.TypeLits


class Sextable a where
  type Elem a
  data Sext (i :: Nat) a

  unsafeCreate :: a -> Sext i a
  unwrap :: Sext i a -> a

  append :: Sext m a -> Sext n a -> Sext (m + n) a
  replicate :: KnownNat m => Elem a -> Sext m a
  map :: (Elem a -> Elem a) -> Sext m a -> Sext m a

  take :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => Sext m a -> Sext n a
  drop :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => Sext m a -> Sext n a


length :: forall a m.
          KnownNat m => Sext m a -> P.Int
length _ = P.fromIntegral P.$ natVal (Proxy :: Proxy m)


padLeft :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (n - m + m), m <= n) =>
           Elem a -> Sext m a -> Sext n a
padLeft pad = append (replicate pad)


padRight :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (m + (n - m)), m <= n) =>
           Elem a -> Sext m a -> Sext n a
padRight pad = P.flip append (replicate pad)
