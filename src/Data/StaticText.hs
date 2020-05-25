{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

{-|

static-text combinators are defined for members of 'IsStaticText'
class. The package includes 'IsStaticText' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.StaticText as S

-}

module Data.StaticText
       (
         -- * Constructing static texts
         --
         -- | See also 'C.unsafeCreate'
         create
       , createLeft
       , createRight
       , st
       , replicate

         -- * Working with static texts
       , append
       , take
       , drop
       , map
       , padLeft
       , padRight

       , length

         -- * IsStaticText class
       , Static
       , IsStaticText(Elem, unsafeCreate, unwrap)
       )

where

import           Prelude as P hiding (drop, length, map, replicate, take)

import           GHC.TypeLits

import           Data.Proxy
import           Data.StaticText.Class (Elem, Static, IsStaticText)
import qualified Data.StaticText.Class as C
import           Data.StaticText.TH


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char (toUpper)


-- | Safely create a Static, possibly altering the source to match
-- target length. If target length is less than that of the source,
-- the source gets truncated. If target length is greater, the source
-- is padded using the provided basic element. Elements on the left
-- are preferred.
--
-- >>> createLeft ' ' "foobarbaz" :: Static String 6
-- "foobar"
-- >>> createLeft '#' "foobarbaz" :: Static String 12
-- "foobarbaz###"
createLeft :: forall a i.
              (IsStaticText a, KnownNat i) =>
              Elem a -> a -> Static a i
createLeft e s =
  C.unsafeCreate $
  C.take t $
  C.append s $
  C.replicate (t - C.length s) e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Just like 'createLeft', except that elements on the right are preferred.
--
-- >>> createRight '@' "foobarbaz" :: Static String 6
-- "barbaz"
-- >>> createRight '!' "foobarbaz" :: Static String 12
-- "!!!foobarbaz"
createRight :: forall a i.
               (IsStaticText a, KnownNat i) =>
               Elem a -> a -> Static a i
createRight e s =
  C.unsafeCreate $
  C.drop (C.length s - t) $
  C.append (C.replicate (t - C.length s) e) s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Safely create a Static if it matches target length.
--
-- >>> create "foobar" :: Maybe (Static String 6)
-- Just "foobar"
-- >>> create "barbaz" :: Maybe (Static String 8)
-- Nothing
--
-- This is safer than 'C.unsafeCreate' and unlike with 'createLeft' /
-- 'createRight' the source value is left unchanged. However, this
-- implies a further run-time check for Nothing values.
create :: forall a i.
          (IsStaticText a, KnownNat i) =>
          a -> P.Maybe (Static a i)
create s =
  if C.length s == t
  then Just $ C.unsafeCreate s
  else Nothing
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Append two Statics together.
--
-- >>> append $(st "foo") $(st "bar") :: Static String 6
-- "foobar"
append :: forall a m n.
          (IsStaticText a) => Static a m -> Static a n -> Static a (m + n)
append a b = C.unsafeCreate $ C.append (C.unwrap a) (C.unwrap b)


-- | Construct a new Static from a basic element.
--
-- >>> replicate '=' :: Static String 10
-- "=========="
replicate :: forall a i.
             (IsStaticText a, KnownNat i) => Elem a -> Static a i
replicate e =
  C.unsafeCreate $ C.replicate t e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Map a Static to a Static of the same length.
--
-- >>> map toUpper $(st "Hello") :: Static String 5
-- "HELLO"
map :: IsStaticText a =>
       (Elem a -> Elem a) -> Static a m -> Static a m
map f s =
  C.unsafeCreate $ C.map f $ C.unwrap s


-- | Reduce Static length, preferring elements on the left.
--
-- >>> take $(st "Foobar") :: Static String 3
-- "Foo"
take :: forall a m n.
        (IsStaticText a, KnownNat m, KnownNat n, n <= m) =>
        Static a m -> Static a n
take s =
  C.unsafeCreate $ C.take t $ C.unwrap s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Reduce Static length, preferring elements on the right.
--
-- >>> drop $(st "Foobar") :: Static String 2
-- "ar"
drop :: forall a m n.
        (IsStaticText a, KnownNat m, KnownNat n, n <= m) =>
        Static a m -> Static a n
drop s =
  C.unsafeCreate $ C.drop (C.length s' - t) s'
  where
    s' = C.unwrap s
    t = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Obtain value-level length.
length :: forall a m.
          KnownNat m => Static a m -> P.Int
length _ = P.fromIntegral P.$ natVal (Proxy :: Proxy m)


-- | Fill a Static with extra elements up to target length, padding
-- original elements to the left.
padLeft :: forall a m n.
           (IsStaticText a, KnownNat m, KnownNat (n - m),
            n ~ (n - m + m), m <= n) =>
           Elem a -> Static a m -> Static a n
padLeft pad = append (replicate pad)


-- | Like 'padLeft', but original elements are padded to the right.
padRight :: forall a m n.
           (IsStaticText a, KnownNat m, KnownNat (n - m),
            n ~ (m + (n - m)), m <= n) =>
           Elem a -> Static a m -> Static a n
padRight pad = P.flip append (replicate pad)
