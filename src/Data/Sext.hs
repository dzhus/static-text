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
       )

where

import           Prelude (($), (.))
import qualified Prelude as P

import qualified Data.Text as T

import           Language.Haskell.TH

import           Data.Sext.Class
import           Data.Sext.TH


mkSext
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


mkSext
  (\_ -> ConT ''T.Text)
  (\_ -> ConT ''P.Char)
  (mkName "Text")
  'T.append
  'textReplicate
  'T.map
  'T.take
  'T.drop
