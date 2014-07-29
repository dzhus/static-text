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
  data SC a (i :: Nat)
  type Elem a

  unsafeCreate :: a -> SC a i
  unwrap :: SC a i -> a

  append :: SC a m -> SC a n -> SC a (m + n)
  replicate :: KnownNat m => Elem a -> SC a m
  map :: (Elem a -> Elem a) -> SC a m -> SC a m

  take :: (KnownNat m, n <= m) => SC a m -> SC a n
  drop :: (KnownNat m, n <= m) => SC a m -> SC a n


length :: forall a m.
          KnownNat m => SC a m -> P.Int
length _ = P.fromIntegral $ natVal (Proxy :: Proxy m)


padLeft :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (n - m + m), m <= n) =>
           Elem a -> SC a m -> SC a n
padLeft pad = append (replicate pad)


padRight :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (m + (n - m)), m <= n) =>
           Elem a -> SC a m -> SC a n
padRight pad = P.flip append (replicate pad)


instance Sextable T.Text where
  data SC T.Text i = Text T.Text
  type Elem T.Text = P.Char

  unsafeCreate = Text

  unwrap (Text s) = s

  append (Text a) (Text b) = Text (T.append a b)

  replicate :: forall m. KnownNat m => Elem T.Text -> SC T.Text m
  replicate c =
    Text $ T.replicate (P.fromIntegral $ natVal (Proxy :: Proxy m)) $ T.singleton c

  map f (Text s) = Text (T.map f s)

  take (Text s :: SC T.Text m) =
    Text $ T.take (P.fromIntegral $ natVal (Proxy :: Proxy m)) s

  drop (Text s :: SC T.Text m) =
    Text $ T.drop ((T.length s) - (P.fromIntegral $ natVal (Proxy :: Proxy m))) s
