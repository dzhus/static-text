{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Sext

where

import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits


data Sext (a :: Nat) = Sext Text deriving Show


-- | Unsafely construct a Sext.
unsafeSext :: Text -> Sext a
unsafeSext = Sext


length :: forall n. KnownNat n => Sext n -> Integer
length _ = natVal (undefined :: Proxy n)


append :: Sext m -> Sext n -> Sext (m + n)
append (Sext a) (Sext b) = Sext $ T.append a b


map :: (Char -> Char) -> Sext m -> Sext m
map f (Sext s) = Sext $ T.map f s


take :: forall n m. (KnownNat n, n <= m) =>
        Sext m -> Sext n
take (Sext s) = Sext $ T.take (fromIntegral $ natVal (undefined :: Proxy n)) s
