{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Use this module when you need to add an 'IsStaticText' instance to a
type.

-}

module Data.StaticText.Class
       ( IsStaticText(..)
       )

where

import           Prelude
import qualified Prelude as P

#ifdef WITH_BS
import qualified Data.ByteString as B
import           GHC.Word
import qualified Data.ByteString.Short as BS
#endif

#ifdef WITH_TEXT
import qualified Data.Text as T
#endif

#ifdef WITH_VECTOR
import qualified Data.Vector as V
#endif

#if MIN_VERSION_base(4,9,0)
import           GHC.TypeLits hiding (Text)
#else
import           GHC.TypeLits
#endif


-- | Class of types which can be assigned a type-level length.
class IsStaticText a where
  -- | Data family which wraps values of the underlying type giving
  -- them a type-level length. @StaticText 6 t@ means a value of type @t@ of
  -- length 6.
  data StaticText (i :: Nat) a

  -- | Basic element type. For @IsStaticText [a]@, this is @a@.
  type Elem a

  -- | Simply wrap a value in a StaticText as is, assuming any length.
  --
  -- For example, an expression like
  --
  -- > unsafeCreate "somestring" :: StaticText 50 String
  --
  -- will typecheck, although the stored length information will not
  -- match actual string size. This may result in wrong behaviour of
  -- all functions defined for StaticText.
  --
  -- Use it only when you know what you're doing.
  --
  -- When implementing new IsStaticText instances, code this to simply
  -- apply the constructor of 'StaticText'.
  unsafeCreate :: a -> StaticText i a

  -- | Forget type-level length, obtaining the underlying value.
  unwrap :: StaticText i a -> a

  length :: a -> Int
  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a


instance (Show a, IsStaticText a) => Show (StaticText i a) where
  show = show . unwrap
  showsPrec p = showsPrec p . unwrap


instance IsStaticText [a] where
  type Elem [a] = a

  data StaticText i [a] = List [a]
    deriving (Eq, Ord)

  unsafeCreate = List
  unwrap (List l) = l

  length = P.length
  append = (P.++)
  replicate = P.replicate
  map = P.map
  take = P.take
  drop = P.drop


#ifdef WITH_TEXT
instance IsStaticText T.Text where
  type Elem T.Text = Char

  data StaticText i T.Text = Text T.Text
    deriving (Eq, Ord)

  unsafeCreate = Text
  unwrap (Text t) = t

  length = T.length
  append = T.append
  replicate = \n c -> T.replicate n (T.singleton c)
  map = T.map
  take = T.take
  drop = T.drop
#endif


#ifdef WITH_BS
instance IsStaticText B.ByteString where
  type Elem B.ByteString = Word8

  data StaticText i B.ByteString = ByteString B.ByteString
    deriving (Eq, Ord)

  unsafeCreate = ByteString
  unwrap (ByteString t) = t

  length = B.length
  append = B.append
  replicate = B.replicate
  map = B.map
  take = B.take
  drop = B.drop

-- | IsStaticText instance for 'BS.ShortByteString' uses intermediate
-- 'B.ByteString's (pinned) for all modification operations.
instance IsStaticText BS.ShortByteString where
  type Elem BS.ShortByteString = Word8

  data StaticText i BS.ShortByteString = ByteStringS BS.ShortByteString
    deriving (Eq, Ord)

  unsafeCreate = ByteStringS
  unwrap (ByteStringS t) = t

  length = BS.length
  append a b = BS.toShort $ B.append (BS.fromShort a) (BS.fromShort b)
  replicate n = BS.toShort . B.replicate n
  map f = BS.toShort . B.map f . BS.fromShort
  take n = BS.toShort . B.take n . BS.fromShort
  drop n = BS.toShort . B.drop n . BS.fromShort
#endif


#ifdef WITH_VECTOR
instance IsStaticText (V.Vector a) where
  type Elem (V.Vector a) = a

  data StaticText i (V.Vector a) = Vector (V.Vector a)
    deriving (Eq, Ord)

  unsafeCreate = Vector
  unwrap (Vector t) = t

  length = V.length
  append = (V.++)
  replicate = V.replicate
  map = V.map
  take = V.take
  drop = V.drop
#endif
