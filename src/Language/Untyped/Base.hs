{-# LANGUAGE DeriveDataTypeable #-}

module Language.Untyped.Base (Info(..)) where

import           Data.Data


data Info = Info { row :: Int, col :: Int } deriving (Eq, Show, Data, Typeable)
