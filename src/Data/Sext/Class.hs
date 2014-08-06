{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Sext.Class
       ( Sextable(..)
       )

where

import qualified Prelude as P

import           GHC.TypeLits


-- | Class of types which can be assigned a type-level length.
class Sextable a where
  -- | Data family which wraps values of the underlying type giving
  -- them a type-level length. @Sext 6 t@ means a value of type @t@ of
  -- length 6.
  --
  -- In examples in this class, we omit constructor names of this type.
  data Sext (i :: Nat) a
  -- | Basic element type. For @Sextable [a]@, this is @a@.
  type Elem a

  -- | Safely create a Sext, possibly altering the source to match
  -- target length. If target length is less than that of the source,
  -- it gets truncated. If target length is greater, the source is
  -- padded using the provided basic element. Elements on the left are
  -- preferred.
  --
  -- >>> createLeft ' ' "foobarbaz" :: Sext 6 String
  -- "foobar"
  -- >>> createLeft '#' "foobarbaz" :: Sext 12 String
  -- "foobarbaz###"
  createLeft :: (KnownNat i) => Elem a -> a -> Sext i a

  -- | Just like 'createLeft', but elements on the right are preferred.
  --
  -- >>> createRight '@' "foobarbaz" :: Sext 6 String
  -- "barbaz"
  -- >>> createRight '!' "foobarbaz" :: Sext 12 String
  -- "!!!foobarbaz"
  createRight :: (KnownNat i) => Elem a -> a -> Sext i a

  -- | Attempt to safely create a Sext if it matches target length.
  --
  -- >>> create "foobar" :: Maybe (Sext 6 String)
  -- Just "foobar"
  -- >>> create "barbaz" :: Maybe (Sext 8 String)
  -- Nothing
  --
  -- This is safer than 'unsafeCreate' and unlike with 'createLeft' /
  -- 'createRight' the source value is left unchanged. However, this
  -- implies a further run-time check for Nothing values.
  create :: (KnownNat i) => a -> P.Maybe (Sext i a)

  -- | Simply wrap a value in a Sext as is, assuming any length.
  --
  -- For example, an expression like
  --
  -- > unsafeCreate "somestring" :: Sext 50 String
  --
  -- will typecheck, although the stored length information will not
  -- match actual string size. This may result in wrong behaviour of
  -- all functions defined for Sext.
  --
  -- Use it only when you know what you're doing.
  unsafeCreate :: a -> Sext i a

  -- | Forget type-level length.
  unwrap :: Sext i a -> a

  append :: Sext m a -> Sext n a -> Sext (m + n) a

  -- | Construct a new Sext from a basic element.
  --
  -- >>> replicate '=' :: Sext 10 String
  -- "=========="
  replicate :: KnownNat m => Elem a -> Sext m a

  map :: (Elem a -> Elem a) -> Sext m a -> Sext m a

  -- | Reduce Sext length, preferring elements on the left.
  --
  -- >>> take "Foobar" :: Sext 3 String
  -- "Foo"
  take :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => Sext m a -> Sext n a
  -- | Reduce Sext length, preferring elements on the right.
  --
  -- >>> drop "Foobar" :: Sext 2 String
  -- "ar"
  drop :: (KnownNat m, KnownNat n, P.True ~ (<=?) n m) => Sext m a -> Sext n a

instance (P.Show a, Sextable a) => P.Show (Sext i a) where
  show s = P.show P.$ unwrap s
