{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE RankNTypes           #-}

module Parser.Parser
  ( discoverItems
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Exception                  (catch)
import           Data.String                        (IsString(..))
import qualified Data.Text                  as T    (pack)
import           Data.Text                          (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL  (unpack)
import qualified Data.HashMap.Strict        as HM   (insert)
import qualified Data.Maybe                 as M    (isJust)
import qualified Data.Char                  as C    (toUpper)
import qualified Data.Time.Clock.POSIX      as Time (posixSecondsToUTCTime)

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types                   (Value(..), Parser, parseEither)
import qualified Network.Wreq               as R
import           Network.Wreq                       (Response, JSONError(..))

import qualified Database.Persist.Class     as P    (insertUnique)

import           Model
import           Parser.Types
import           Import                             (DB)
import           Yesod.Core.Types                   (Logger, loggerPutStr)

default (Text)

type URL = String
type MaxItemId  = Int

-- Asks for the current largest item id and
-- makes a particular number of steps backward
-- from that point.
discoverItems :: Logger -> Int -> DB [Item]
discoverItems logger itemsAmount = do
  maxitemId <- liftIO $ getCurrentMaxitem
  worker logger itemsAmount maxitemId
  where worker :: Logger -> Int -> MaxItemId -> DB [Item]
        worker logger counter itemId
         | counter == 0 = return []
         | otherwise =
             let url = "https://hacker-news.firebaseio.com/v0/item/" ++ show itemId ++ ".json"
             in do item <- liftIO $ parseItem logger url
                   liftIO $ loggerPutStr logger $ "Item parsed: " <> (fromString . show $ item) <> "\n"
                   let go = worker logger (pred counter) (pred itemId)
                   case item of
                     Nothing -> go -- TODO: check if in the database + additional param
                     Just item' -> do P.insertUnique item' -- TODO: one-to-many kids
                                      rest <- go
                                      return $ item' : rest

getCurrentMaxitem :: IO MaxItemId
getCurrentMaxitem =
  let url = "https://hacker-news.firebaseio.com/v0/maxitem.json"
  in do resp <- R.get url
        let maxitemId = read . BSL.unpack $ resp ^. R.responseBody
        putStrLn "MAX ITEM:"
        print maxitemId
        return maxitemId


instance FromJSON Item where
  parseJSON (Object obj) = do
    itemApiId <- T.pack . show <$> (obj .: "id" :: Parser Int)
    itemDeleted <- obj `getBool` "deleted"
    itemTypeText <- obj .: "type"
    let itemItemType = read $ (C.toUpper . head $ itemTypeText) : tail itemTypeText :: ItemType
    itemUsername <- obj .: "by"
    itemCreated <- toUTC <$> obj .: "time"
    itemText <- obj .:? "text"
    itemDead <- obj `getBool` "dead"
    itemParent' <- obj .:? "parent" :: Parser (Maybe Int)
    let itemParent = T.pack . show <$> itemParent'
    itemPool <- obj .:? "pool"
    itemUrl <- obj .: "url"
    itemScore <- obj .:? "score"
    itemTitle <- obj .:? "title"
    return Item{..}

    where getBool obj key = do
            v <- obj .:? key :: Parser (Maybe Text)
            return $ M.isJust v
          toUTC = Time.posixSecondsToUTCTime
  parseJSON _ = error "Not an object."


parseItem :: Logger -> URL -> IO (Maybe Item)
parseItem logger url = do
  mResp <- flip catch (\(JSONError _) -> return Nothing) $
             Just <$> (R.asJSON =<< R.get url :: IO (Response Object))
  case mResp of
    Nothing -> loggerPutStr logger "Bad response.\n" >> return Nothing
    Just resp ->
      let respBody = HM.insert "url" (String . fromString $ url) (resp ^. R.responseBody)
      in case (parseEither parseJSON $ Object respBody) of
           Left e -> loggerPutStr logger (fromString e <> "\n") >> return Nothing
           Right item -> return $ Just item
