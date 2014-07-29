{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Sext
       ( Sextable(..)
       , length
       , padLeft
       , padRight
       )

where

import           Prelude (($), (-))
import qualified Prelude as P

import           Data.Proxy
import qualified Data.Text as T

import           GHC.TypeLits


class Sextable a where
  data SC (i :: Nat) a
  type Elem a

  unsafeCreate :: a -> SC i a
  unwrap :: SC i a -> a

  append :: SC m a -> SC n a -> SC (m + n) a
  replicate :: KnownNat m => Elem a -> SC m a
  map :: (Elem a -> Elem a) -> SC m a -> SC m a

  take :: (KnownNat m, n <= m) => SC m a -> SC n a
  drop :: (KnownNat m, n <= m) => SC m a -> SC n a


length :: forall a m.
          KnownNat m => SC m a -> P.Int
length _ = P.fromIntegral $ natVal (Proxy :: Proxy m)


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


instance Sextable T.Text where
  data SC i T.Text = Text T.Text
  type Elem T.Text = P.Char

  unsafeCreate = Text

  unwrap (Text s) = s

  append (Text a) (Text b) = Text (T.append a b)

  replicate :: forall m. KnownNat m => Elem T.Text -> SC m T.Text
  replicate c =
    Text $ T.replicate (P.fromIntegral $ natVal (Proxy :: Proxy m)) $ T.singleton c

  map f (Text s) = Text (T.map f s)

  take (Text s :: SC m T.Text) =
    Text $ T.take (P.fromIntegral $ natVal (Proxy :: Proxy m)) s

  drop (Text s :: SC m T.Text) =
    Text $ T.drop ((T.length s) - (P.fromIntegral $ natVal (Proxy :: Proxy m))) s
