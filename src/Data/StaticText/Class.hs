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
  -- them a type-level length. @Static t 6@ means a value of type @t@ of
  -- length 6.
  data Static a (i :: Nat)

  -- | Basic element type. For @IsStaticText [a]@, this is @a@.
  type Elem a

  -- | Simply wrap a value in a Static as is, assuming any length.
  --
  -- __WARNING__ Use it only when you know what you're doing.
  --
  -- For example, an expression like
  --
  -- > unsafeCreate "somestring" :: Static String 50
  --
  -- will typecheck, although the stored length information will not
  -- match actual string size. This may result in wrong behaviour of
  -- all functions defined for "IsStaticText".
  --
  -- When writing new "IsStaticText" instances, make this simply apply
  -- the constructor of "Static".
  unsafeCreate :: a -> Static a i

  -- | Forget type-level length, obtaining the underlying value.
  unwrap :: Static a i -> a

  length :: a -> Int
  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a


instance (Show a, IsStaticText a) => Show (Static a i) where
  show = show . unwrap
  showsPrec p = showsPrec p . unwrap


instance IsStaticText [a] where
  type Elem [a] = a

  data Static [a] i = List [a]
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

  data Static T.Text i = Text T.Text
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

  data Static B.ByteString i = ByteString B.ByteString
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

  data Static BS.ShortByteString i = ByteStringS BS.ShortByteString
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

  data Static (V.Vector a) i = Vector (V.Vector a)
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
