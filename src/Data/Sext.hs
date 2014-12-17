{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|

Sext (/s/tatic t/ext/) provides type-level safety for basic operations
on string-like types (finite lists of elements). Use it when you need
static guarantee on lengths of strings produced in your code.

An example application would be a network exchange protocol built of
packets with fixed-width fields:

> mkPacket :: String -> Sext 32 String
> mkPacket inp =
>   -- 5-character version signature
>   $(sext "PKT10") `append`
>   -- 25-character payload
>   payload `append`
>   -- 2-character payload checksum
>   checksum
>   where
>     payload = createLeft ' ' inp
>     checksum :: Sext 2 String
>     checksum = createLeft ' ' $
>                show $ length payload `mod` 100
>
> message :: Sext 64 String
> message = mkPacket "Hello" `append` mkPacket "world"

Sext combinators are defined for members of 'Sextable' class. The
package includes 'Sextable' instances for several common types.

This module is meant to be imported qualifed, e.g.

> import qualified Data.Sext as S

-}

module Data.Sext
       (
         -- * Constructing Sexts
         --
         -- | See also 'C.unsafeCreate'
         createLeft
       , createRight
       , sext
       , create
       , replicate

         -- * Working with Sexts
       , append
       , take
       , drop
       , map
       , padLeft
       , padRight

       , length

         -- * Sextable class
       , Sextable(C.unsafeCreate, C.unwrap)
       )

where

import           Prelude as P hiding (drop, length, map, replicate, take)
import qualified Prelude as P hiding (length)

import           GHC.TypeLits

import           Data.Proxy
import           Data.Sext.Class (Elem, Sext, Sextable)
import qualified Data.Sext.Class as C
import           Data.Sext.TH


-- | Safely create a Sext, possibly altering the source to match
-- target length. If target length is less than that of the source,
-- the source gets truncated. If target length is greater, the source
-- is padded using the provided basic element. Elements on the left
-- are preferred.
--
-- >>> createLeft ' ' "foobarbaz" :: Sext 6 String
-- "foobar"
-- >>> createLeft '#' "foobarbaz" :: Sext 12 String
-- "foobarbaz###"
createLeft :: forall a i.
              (Sextable a, KnownNat i) =>
              Elem a -> a -> Sext i a
createLeft e s =
  C.unsafeCreate $
  C.take (C.length s) $
  C.append s $
  C.replicate (t - C.length s) e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Just like 'createLeft', except that elements on the right are preferred.
--
-- >>> createRight '@' "foobarbaz" :: Sext 6 String
-- "barbaz"
-- >>> createRight '!' "foobarbaz" :: Sext 12 String
-- "!!!foobarbaz"
createRight :: forall a i.
               (Sextable a, KnownNat i) =>
               Elem a -> a -> Sext i a
createRight e s =
  C.unsafeCreate $
  C.drop (C.length s - t) $
  C.append (C.replicate (t - C.length s) e) s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Attempt to safely create a Sext if it matches target length.
--
-- >>> create "foobar" :: Maybe (Sext 6 String)
-- Just "foobar"
-- >>> create "barbaz" :: Maybe (Sext 8 String)
-- Nothing
--
-- This is safer than 'C.unsafeCreate' and unlike with 'createLeft' /
-- 'createRight' the source value is left unchanged. However, this
-- implies a further run-time check for Nothing values.
create :: forall a i.
          (Sextable a, KnownNat i) =>
          a -> P.Maybe (Sext i a)
create s =
  if C.length s == t
  then Just $ C.unsafeCreate s
  else Nothing
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


-- | Append two Sexts together.
--
-- >>> append "foo" "bar" :: Sext 6 String
-- "foobar"
append :: forall a m n.
          (Sextable a) => Sext m a -> Sext n a -> Sext (m + n) a
append a b = C.unsafeCreate $ C.append (C.unwrap a) (C.unwrap b)


-- | Construct a new Sext from a basic element.
--
-- >>> replicate '=' :: Sext 10 String
-- "=========="
replicate :: forall a i.
             (Sextable a, KnownNat i) => Elem a -> Sext i a
replicate e =
  C.unsafeCreate $ C.replicate t e
  where
    t = fromIntegral $ natVal (Proxy :: Proxy i)


map :: Sextable a =>
       (Elem a -> Elem a) -> Sext m a -> Sext m a
map f s =
  C.unsafeCreate $ C.map f $ C.unwrap s


-- | Reduce Sext length, preferring elements on the left.
--
-- >>> take "Foobar" :: Sext 3 String
-- "Foo"
take :: forall a m n.
        (Sextable a, KnownNat m, KnownNat n, n <= m) =>
        Sext m a -> Sext n a
take s =
  C.unsafeCreate $ C.take t $ C.unwrap s
  where
    t = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Reduce Sext length, preferring elements on the right.
--
-- >>> drop "Foobar" :: Sext 2 String
-- "ar"
drop :: forall a m n.
        (Sextable a, KnownNat m, KnownNat n, n <= m) =>
        Sext m a -> Sext n a
drop s =
  C.unsafeCreate $ C.drop (C.length s' - t) s'
  where
    s' = C.unwrap s
    t = fromIntegral $ natVal (Proxy :: Proxy n)


-- | Obtain value-level length.
length :: forall a m.
          KnownNat m => Sext m a -> P.Int
length _ = P.fromIntegral P.$ natVal (Proxy :: Proxy m)


-- | Fill a Sext with extra elements up to target length, padding
-- original elements to the left.
padLeft :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (n - m + m), m <= n) =>
           Elem a -> Sext m a -> Sext n a
padLeft pad = append (replicate pad)


-- | Like 'padLeft', but original elements are padded to the right.
padRight :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (m + (n - m)), m <= n) =>
           Elem a -> Sext m a -> Sext n a
padRight pad = P.flip append (replicate pad)
