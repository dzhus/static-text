{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Use this only if you need to make some type Sextable.

-}

module Data.Sext.Class
       ( Sextable(..)
       )

where

import           Prelude
import qualified Prelude as P

#ifdef WITH_BS
import qualified Data.ByteString as B
import           GHC.Word
#endif

#ifdef WITH_TEXT
import qualified Data.Text as T
#endif

#if MIN_VERSION_base(4,9,0)
import           GHC.TypeLits hiding (Text)
#else
import           GHC.TypeLits
#endif


-- | Class of types which can be assigned a type-level length.
class Sextable a where
  -- | Data family which wraps values of the underlying type giving
  -- them a type-level length. @Sext 6 t@ means a value of type @t@ of
  -- length 6.
  data Sext (i :: Nat) a

  -- | Basic element type. For @Sextable [a]@, this is @a@.
  type Elem a

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
  --
  -- When implementing new Sextable instances, code this to simply
  -- apply the constructor of 'Sext'.
  unsafeCreate :: a -> Sext i a

  -- | Forget type-level length, obtaining the underlying value.
  unwrap :: Sext i a -> a

  length :: a -> Int
  append :: a -> a -> a
  replicate :: Int -> Elem a -> a
  map :: (Elem a -> Elem a) -> a -> a
  take :: Int -> a -> a
  drop :: Int -> a -> a


instance (Show a, Sextable a) => Show (Sext i a) where
  show s = show $ unwrap s


instance Sextable [a] where
  type Elem [a] = a

  data Sext i [a] = List [a]

  unsafeCreate = List
  unwrap (List l) = l

  length = P.length
  append = (P.++)
  replicate = P.replicate
  map = P.map
  take = P.take
  drop = P.drop


#ifdef WITH_TEXT
instance Sextable T.Text where
  type Elem T.Text = Char

  data Sext i T.Text = Text T.Text

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
instance Sextable B.ByteString where
  type Elem B.ByteString = Word8

  data Sext i B.ByteString = ByteString B.ByteString

  unsafeCreate = ByteString
  unwrap (ByteString t) = t

  length = B.length
  append = B.append
  replicate = B.replicate
  map = B.map
  take = B.take
  drop = B.drop
#endif
