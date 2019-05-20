{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Parser.Types
  ( ItemType(..)
  ) where

import           Database.Persist.TH

import           Data.Data    (Typeable, Data, toConstr, showConstr)

import           GHC.Generics (Generic)
import           Data.Aeson   (ToJSON(..))

data ItemType =
    Job
  | Story
  | Comment
  | Pool
  | Poolopt
  deriving (Typeable, Eq, Data, Read, Generic)
derivePersistField "ItemType"

instance Show ItemType where
  show = showConstr . toConstr

instance ToJSON ItemType where
