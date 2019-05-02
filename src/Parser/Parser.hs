{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}

module Parser.Parser where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Reader         (ReaderT)
import           Control.Exception                  (catch)
import qualified Data.Text                  as T    (pack)
import           Data.Text                          (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL  (unpack)
import qualified Data.Maybe                 as M    (isJust)
import qualified Data.Char                  as C    (toUpper)
import qualified Data.Time.Clock.POSIX      as Time (posixSecondsToUTCTime)

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types                   (Value(..), Parser, parseMaybe)
import qualified Network.Wreq               as R
import           Network.Wreq                       (Response, JSONError(..))

import           Data.Pool                          (Pool)
import qualified Database.Persist.Class     as P    (insertUnique)
import qualified Database.Persist.Sql       as P    (runSqlPool)
import           Database.Persist.Sql               (SqlBackend)

import           Model
import           Parser.Types

default (Text)

type URL = String
type MaxItemId  = Int

-- Asks for the current largest item id and
-- makes a particular number of steps backward
-- from that point.
discoverItems :: Int -> Pool SqlBackend -> IO ()
discoverItems itemsAmount connPool = do
  maxitemId <- getCurrentMaxitem
  flip P.runSqlPool connPool $ worker itemsAmount maxitemId
  where worker :: Int -> MaxItemId -> ReaderT SqlBackend IO ()
        worker 0 _ = return ()
        worker counter itemId =
          let url = "https://hacker-news.firebaseio.com/v0/item/" ++ show itemId ++ ".json"
          in do item <- liftIO $ parseItem url
                let go = worker (pred counter) (pred itemId)
                case item of
                  Nothing -> go -- TODO: check if in the database + additional param
                  Just item' -> P.insertUnique item' >> go -- TODO: one-to-many kids

getCurrentMaxitem :: IO MaxItemId
getCurrentMaxitem =
  let url = "https://hacker-news.firebaseio.com/v0/maxitem.json"
  in do resp <- R.get url
        let maxitemId = read . BSL.unpack $ resp ^. R.responseBody
        return maxitemId


instance FromJSON (URL -> Item) where
  parseJSON (Object obj) = do
    itemApiId <- obj .: "id"
    itemDeleted <- obj `getBool` "deleted"
    itemTypeText <- obj .: "type"
    let itemItemType = read $ (C.toUpper . head $ itemTypeText) : tail itemTypeText :: ItemType
    itemUsername <- obj .: "by"
    itemCreated <- toUTC <$> obj .: "time"
    itemText <- obj .:? "text"
    itemDead <- obj `getBool` "dead"
    itemParent <- obj .:? "parent"
    itemPool <- obj .:? "pool"
    itemScore <- obj .: "score"
    itemTitle <- obj .:? "title"
    return $ \strItemUrl -> let itemUrl = T.pack strItemUrl in Item{..}

    where getBool obj key = do
            v <- obj .:? key :: Parser (Maybe Text)
            return $ M.isJust v
          toUTC = Time.posixSecondsToUTCTime
  parseJSON _ = error "Not an object."


parseItem :: URL -> IO (Maybe Item)
parseItem url = do
  mResp <- flip catch (\(JSONError _) -> return Nothing) $
             Just <$> (R.asJSON =<< R.get url :: IO (Response Object))
  return $
    case mResp of
      Nothing -> Nothing
      Just resp -> let respBody = resp ^. R.responseBody
                       toItem   = parseMaybe parseJSON (Object respBody)
                   in case toItem of
                        Nothing -> Nothing
                        Just f -> Just (f url)
