{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Sext
       ( Sextable(..)
       , length
       , padLeft
       , padRight
       , sext
       )

where

import qualified Prelude as P

#ifdef WITH_BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           GHC.Word
#endif

#ifdef WITH_TEXT
import qualified Data.Text as T
#endif

import           GHC.TypeLits

import           Language.Haskell.TH

import           Data.Proxy
import           Data.Sext.Class
import           Data.Sext.TH


length :: forall a m.
          KnownNat m => Sext m a -> P.Int
length _ = P.fromIntegral P.$ natVal (Proxy :: Proxy m)


padLeft :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (n - m + m), m <= n) =>
           Elem a -> Sext m a -> Sext n a
padLeft pad = append (replicate pad)


padRight :: forall a m n.
           (Sextable a, KnownNat m, KnownNat (n - m),
            n ~ (m + (n - m)), m <= n) =>
           Elem a -> Sext m a -> Sext n a
padRight pad = P.flip append (replicate pad)


mkSextable
  (AppT ListT P.. VarT)
  (VarT)
  (mkName "List")
  'P.length
  '(P.++)
  'P.replicate
  'P.map
  'P.take
  'P.drop


#ifdef WITH_TEXT
textReplicate :: P.Int -> P.Char -> T.Text
textReplicate n c = T.replicate n P.$ T.singleton c


mkSextable
  (\_ -> ConT ''T.Text)
  (\_ -> ConT ''P.Char)
  (mkName "Text")
  'T.length
  'T.append
  'textReplicate
  'T.map
  'T.take
  'T.drop
#endif


#ifdef WITH_BS
mkSextable
  (\_ -> ConT ''B.ByteString)
  (\_ -> ConT ''Word8)
  (mkName "ByteString")
  'B.length
  'B.append
  'B.replicate
  'B.map
  'B.take
  'B.drop


mkSextable
  (\_ -> ConT ''LB.ByteString)
  (\_ -> ConT ''Word8)
  (mkName "LazyByteString")
  'LB.length
  'LB.append
  'LB.replicate
  'LB.map
  'LB.take
  'LB.drop
#endif
