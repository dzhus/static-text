{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
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

import           Prelude (($), (.))
import qualified Prelude as P

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

import           GHC.Word

import           Language.Haskell.TH

import           Data.Sext.Class
import           Data.Sext.TH


mkSextable
  (AppT ListT . VarT)
  (VarT)
  (mkName "List")
  '(P.++)
  'P.replicate
  'P.map
  'P.take
  'P.drop


textReplicate :: P.Int -> P.Char -> T.Text
textReplicate n c = T.replicate n $ T.singleton c


mkSextable
  (\_ -> ConT ''T.Text)
  (\_ -> ConT ''P.Char)
  (mkName "Text")
  'T.append
  'textReplicate
  'T.map
  'T.take
  'T.drop


mkSextable
  (\_ -> ConT ''B.ByteString)
  (\_ -> ConT ''Word8)
  (mkName "ByteString")
  'B.append
  'B.replicate
  'B.map
  'B.take
  'B.drop


mkSextable
  (\_ -> ConT ''LB.ByteString)
  (\_ -> ConT ''Word8)
  (mkName "LazyByteString")
  'LB.append
  'LB.replicate
  'LB.map
  'LB.take
  'LB.drop
