{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Parser.Types
  ( Interval
  , ItemType(..)
  ) where

import           Database.Persist.TH

import           Data.Char (toLower)
import           Data.Data (Typeable, Data, toConstr, showConstr)

{-

data User -- TODO: UserM monad

-- Each source produces its own elements that could be restricted by particular parameters.
-- Source specifies where the elements are.
-- Parameters specify which elements to extract.
class ResourceClass source element | source -> element where
  data ResourceParameters element :: *
  requestElements :: (Monad m) => source -> ResourceParameters element -> m [element]

-- TODO: resource combinations
-- 2 resources with the same resourceSource could be combined
-- that combination will force a combination of their correspondent parameters

data Monitor source = Monitor
  { monitorUser :: User
  , monitorTarget :: source
  , monitorInterval :: Int }
-}

type Interval = Int

data ItemType =
    Job
  | Story
  | Comment
  | Pool
  | Poolopt
  deriving (Typeable, Data, Read)
derivePersistField "ItemType"

instance Show ItemType where
  show = showConstr . toConstr
