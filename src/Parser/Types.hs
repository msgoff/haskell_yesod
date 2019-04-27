{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Parser.Types where

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

type Interval = Int
