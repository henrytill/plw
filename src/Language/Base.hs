{-# LANGUAGE DeriveDataTypeable #-}

module Language.Base
  ( Info(..)
  , infoFrom
  ) where

import Data.Data
import Text.Parsec (SourcePos, sourceColumn, sourceLine)


data Info = Info { row :: Int, col :: Int } deriving (Eq, Show, Data, Typeable)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)
