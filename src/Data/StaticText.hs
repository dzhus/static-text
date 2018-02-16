{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

{-|

static-text provides type-level safety for basic operations on
string-like types (finite lists of elements), such as "Data.Text",
"String" (and all lists), "Data.ByteString" and "Data.Vector". Use it
when you need static guarantee on lengths of strings produced in your
code.

An example application would be a network exchange protocol built of
packets with fixed-width fields:

@
{\-\# LANGUAGE DataKinds #-\}
{\-\# LANGUAGE OverloadedStrings #-\}
{\-\# LANGUAGE TemplateHaskell #-\}
@

> import Data.StaticText
>
> mkPacket :: ByteString -> Static 32 ByteString
> mkPacket inp =
>   -- 5-character version signature
>   $(sext "PKT10") `append`
>   -- 25-character payload
>   payload `append`
>   -- 2-character payload checksum
>   checksum
>   where
>     payload = createLeft 0x20 inp
>     checksum :: Static 2 ByteString
>     checksum = createLeft 0x20 $
>                pack $ show $ Data.Static.length payload `mod` 100
>
> message :: Static 64 ByteString
> message = mkPacket "Hello" `append` mkPacket "world"

static-text combinators are defined for members of 'IsStaticText'
class. The package includes 'IsStaticText' instances for several
common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.StaticText as S

-}

module Data.StaticText
       (
         -- * Constructing StaticTexts
         --
         -- | See also 'C.unsafeCreate'
         createLeft
       , createRight
       , sext
       , create
       , replicate

         -- * Working with StaticTexts
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
-- >>> createLeft ' ' "foobarbaz" :: Static 6 String
-- "foobar"
-- >>> createLeft '#' "foobarbaz" :: Static 12 String
-- "foobarbaz###"
createLeft :: forall a i.
              (IsStaticText a, KnownNat i) =>
              Elem a -> a -> Static i a
createLeft e s =
  C.unsafeCreate $
  C.take t $
  C.append s $
  C.replicate (t - C.length s) e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Just like 'createLeft', except that elements on the right are preferred.
--
-- >>> createRight '@' "foobarbaz" :: Static 6 String
-- "barbaz"
-- >>> createRight '!' "foobarbaz" :: Static 12 String
-- "!!!foobarbaz"
createRight :: forall a i.
               (IsStaticText a, KnownNat i) =>
               Elem a -> a -> Static i a
createRight e s =
  C.unsafeCreate $
  C.drop (C.length s - t) $
  C.append (C.replicate (t - C.length s) e) s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Attempt to safely create a Static if it matches target length.
--
-- >>> create "foobar" :: Maybe (Static 6 String)
-- Just "foobar"
-- >>> create "barbaz" :: Maybe (Static 8 String)
-- Nothing
--
-- This is safer than 'C.unsafeCreate' and unlike with 'createLeft' /
-- 'createRight' the source value is left unchanged. However, this
-- implies a further run-time check for Nothing values.
create :: forall a i.
          (IsStaticText a, KnownNat i) =>
          a -> P.Maybe (Static i a)
create s =
  if C.length s == t
  then Just $ C.unsafeCreate s
  else Nothing
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Append two Statics together.
--
-- >>> append $(sext "foo") $(sext "bar") :: Static 6 String
-- "foobar"
append :: forall a m n.
          (IsStaticText a) => Static m a -> Static n a -> Static (m + n) a
append a b = C.unsafeCreate $ C.append (C.unwrap a) (C.unwrap b)


-- | Construct a new Static from a basic element.
--
-- >>> replicate '=' :: Static 10 String
-- "=========="
replicate :: forall a i.
             (IsStaticText a, KnownNat i) => Elem a -> Static i a
replicate e =
  C.unsafeCreate $ C.replicate t e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Map a Static to a Static of the same length.
--
-- >>> map toUpper $(sext "Hello") :: Static 5 String
-- "HELLO"
map :: IsStaticText a =>
       (Elem a -> Elem a) -> Static m a -> Static m a
map f s =
  C.unsafeCreate $ C.map f $ C.unwrap s


-- | Reduce Static length, preferring elements on the left.
--
-- >>> take $(sext "Foobar") :: Static 3 String
-- "Foo"
take :: forall a m n.
        (IsStaticText a, KnownNat m, KnownNat n, n <= m) =>
        Static m a -> Static n a
take s =
  C.unsafeCreate $ C.take t $ C.unwrap s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Reduce Static length, preferring elements on the right.
--
-- >>> drop $(sext "Foobar") :: Static 2 String
-- "ar"
drop :: forall a m n.
        (IsStaticText a, KnownNat m, KnownNat n, n <= m) =>
        Static m a -> Static n a
drop s =
  C.unsafeCreate $ C.drop (C.length s' - t) s'
  where
    s' = C.unwrap s
    t = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Obtain value-level length.
length :: forall a m.
          KnownNat m => Static m a -> P.Int
length _ = P.fromIntegral P.$ natVal (Proxy :: Proxy m)


-- | Fill a Static with extra elements up to target length, padding
-- original elements to the left.
padLeft :: forall a m n.
           (IsStaticText a, KnownNat m, KnownNat (n - m),
            n ~ (n - m + m), m <= n) =>
           Elem a -> Static m a -> Static n a
padLeft pad = append (replicate pad)


-- | Like 'padLeft', but original elements are padded to the right.
padRight :: forall a m n.
           (IsStaticText a, KnownNat m, KnownNat (n - m),
            n ~ (m + (n - m)), m <= n) =>
           Elem a -> Static m a -> Static n a
padRight pad = P.flip append (replicate pad)
