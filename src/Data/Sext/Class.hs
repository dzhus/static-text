{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Sext.Class
       ( Sextable(..)
       , length
       , padLeft
       , padRight
       )

where

import           Prelude (($), (-), (.), (=<<))
import qualified Prelude as P

import           Data.Proxy
import qualified Data.Text as T

import           GHC.TypeLits


class Sextable a where
  type Elem a
  data SC (i :: Nat) a

  unsafeCreate :: a -> SC i a
  unwrap :: SC i a -> a

  append :: SC m a -> SC n a -> SC (m + n) a
  replicate :: KnownNat m => Elem a -> SC m a
  map :: (Elem a -> Elem a) -> SC m a -> SC m a

  take :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => SC m a -> SC n a
  drop :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => SC m a -> SC n a


length :: forall a m.
          KnownNat m => SC m a -> P.Int
length _ = P.fromIntegral P.$ natVal (Proxy :: Proxy m)


padLeft :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (n - m + m), m <= n) =>
           Elem a -> SC m a -> SC n a
padLeft pad = append (replicate pad)


padRight :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (m + (n - m)), m <= n) =>
           Elem a -> SC m a -> SC n a
padRight pad = P.flip append (replicate pad)
