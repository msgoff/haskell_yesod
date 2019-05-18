{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RankNTypes           #-}

module Parser.Parser
  ( FullItem(..)
  , discoverItems
  , runMonitor
  , fullItemsFromFile
  ) where

import           Control.Monad                      (forever, void)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Class          (lift)
import           Control.Concurrent                 (ThreadId, threadDelay, forkIO, killThread)
import qualified Control.Concurrent.Async   as A    (async, wait)
import           Control.Monad.Coroutine            (Coroutine, resume)
import Control.Monad.Coroutine.SuspensionFunctors   (Yield(..), yield)
import           Control.Exception                  (catch)
import           Data.String                        (IsString(..))
import           Data.IORef                         (newIORef, readIORef, atomicWriteIORef)
import qualified Data.Text                  as T    (pack)
import           Data.Text                          (Text)
import           Data.Proxy                         (Proxy(..))
import qualified Data.ByteString.Lazy.Char8 as BSL  (unpack)
import qualified Data.HashMap.Strict        as HM   (insert, member, lookup)
import qualified Data.Vector                as V    (toList, map)
import           Data.Vector                        (Vector)
import qualified Data.Maybe                 as M    (isJust, maybe, catMaybes)
import qualified Data.Char                  as C    (toUpper)
import qualified Data.Time.Clock.POSIX      as Time (posixSecondsToUTCTime)

import qualified Control.Lens               as L    ((^.))
import           Data.Aeson
import           Data.Aeson.Types                   (Value(..), Parser, parseEither)
import qualified Network.Wreq               as R
import           Network.Wreq                       (Response, JSONError(..))

import qualified Database.Persist.Class     as P    (insertUnique)
import           Database.Persist.Class             (PersistEntity(..))
import           Database.Persist.Types             (PersistValue(PersistInt64))
import           Database.Esqueleto hiding (Value(..))

import           Model
import           Parser.Types
import           Import                             (Handler, DB)
import qualified Import                     as I
import           Yesod.Core.Types                   (Logger, loggerPutStr)

default (Text)

type URL = String
type MaxItemId  = Int
type Interval = Int
type ChunkSize = Int

-- Represents an Item that is ready to be stored in a database
-- and to form a FullItem.
type RawItem = (Item, Value)

data FullItem = FullItem Item [Kid] [Part]
  deriving (Show)

-- Monitor Coroutine
----------------------------------------------------------------------------------------------------

runMonitor :: ChunkSize -> Interval -> Maybe Interval -> Handler ThreadId
runMonitor itemsAmount interval releaseAfter = do
  runInnerHandler <- I.handlerToIO
  monitorThreadId <- liftIO . forkIO . runInnerHandler $
    worker (mkDiscoverStep itemsAmount) interval
  case releaseAfter of
    Nothing -> return ()
    Just releaseAfter' -> registerKillerProcess monitorThreadId releaseAfter'
  return monitorThreadId
  where worker gen interval = do
          Left (Yield _ cont) <- resume gen
          liftIO $ threadDelay (toSeconds interval)
          worker cont interval
        toSeconds = (*) 1000000
        registerKillerProcess threadId stop = void . liftIO . forkIO $ do
          threadDelay (toSeconds stop)
          killThread threadId

mkDiscoverStep :: Int -> Coroutine (Yield [Item]) Handler ()
mkDiscoverStep itemsAmount = do
  dbId <- dbSmallestId
  start <- liftIO $ M.maybe getCurrentMaxitem return dbId >>= newIORef
  logger <- lift $ I.appLogger <$> I.getYesod
  forever $ do
    startValue <- lift $ liftIO $ readIORef start
    lift $ liftIO $ putStrLn "START: " >> print startValue
    items <- lift $ I.runDB $ discoverItems logger (Just startValue) itemsAmount
    lift $ liftIO $ atomicWriteIORef start (startValue - itemsAmount)
    yield items
  where dbSmallestId = lift $ do
          res <- (I.runDB $ select $
                           from $ \item -> do
                           orderBy [asc (item ^. I.ItemApiId)]
                           return item) :: Handler [Entity Item]
          case res of
            [] -> return Nothing
            _ -> return $ Just (I.itemApiId . entityVal . head $ res)


-- Items Scraping
----------------------------------------------------------------------------------------------------

-- Asks for the current largest item id and
-- makes a particular number of steps backward from that point.
-- Parameter 'start' stays for maxitem when defined.
discoverItems :: Logger -> Maybe MaxItemId -> ChunkSize -> DB [Item]
discoverItems logger start itemsAmount = do
  maxitemId <- liftIO $ M.maybe getCurrentMaxitem return start
  itemsAsync <- liftIO $ worker logger itemsAmount maxitemId
  items <- liftIO $ mapM A.wait itemsAsync
  return . M.catMaybes =<< flip mapM items (\item ->
    case item of
      Nothing -> return Nothing
      Just (item', valItem) -> do dbItemKey <- P.insertUnique item'
                                  case dbItemKey of
                                    Nothing -> return Nothing
                                    Just key -> do
                                      let extendedValItem = extendObjectWithModelId key valItem
                                          kids = parseKids extendedValItem
                                          parts = parseParts extendedValItem
                                      mapM_ P.insertUnique kids
                                      mapM_ P.insertUnique parts
                                      return (Just item'))
  where worker logger counter itemId
         | counter == 0 = return []
         | otherwise =
             let url = "https://hacker-news.firebaseio.com/v0/item/" ++ show itemId ++ ".json"
             in do item <- A.async $ parseItem logger url
                   rest <- worker logger (pred counter) (pred itemId)
                   return $ item : rest

getCurrentMaxitem :: IO MaxItemId
getCurrentMaxitem =
  let url = "https://hacker-news.firebaseio.com/v0/maxitem.json"
  in do resp <- R.get url
        let maxitemId = read . BSL.unpack $ resp L.^. R.responseBody
        putStrLn "MAX ITEM:"
        print maxitemId
        return maxitemId


-- Items Parsing
----------------------------------------------------------------------------------------------------

instance ToJSON Item where
instance FromJSON Item where
  parseJSON (Object obj) = do
    itemApiId <- obj .: "id"
    itemDeleted <- obj `getBool` "deleted"
    itemTypeText <- obj .: "type"
    let itemItemType = read $ ((C.toUpper . head $ itemTypeText) : tail itemTypeText) :: ItemType
    itemUsername <- obj .:? "by"
    itemCreated <- toUTC <$> obj .: "time"
    itemText <- obj .:? "text"
    itemDead <- obj `getBool` "dead"
    itemParent' <- obj .:? "parent" :: Parser (Maybe Int)
    let itemParent = T.pack . show <$> itemParent'
    itemPoll <- obj .:? "pool"
    itemUrl <- obj .: "url"
    itemScore <- obj .:? "score"
    itemTitle <- obj .:? "title"
    itemDescendants <- obj .:? "descendants"
    return Item{..}

    where getBool obj key = do
            v <- obj .:? key :: Parser (Maybe Bool)
            return $ M.isJust v
          toUTC = Time.posixSecondsToUTCTime
  parseJSON _ = error "Not an object."

instance {-# OVERLAPPING #-} FromJSON [Kid] where
  parseJSON (Object obj) = do
    itemApiId <- _getItemId obj
    kids <- V.toList <$> obj .: "kids"
    return $ map (Kid itemApiId) kids

instance {-# OVERLAPPING #-} FromJSON [Part] where
  parseJSON (Object obj) = do
    itemApiId <- _getItemId obj
    parts <- V.toList <$> obj .: "parts"
    return $ map (Part itemApiId) parts

parseItem :: Logger -> URL -> IO (Maybe (Item, Value))
parseItem logger url = do
  mResp <- flip catch (\(JSONError _) -> return Nothing) $
             Just <$> (R.asJSON =<< R.get url :: IO (Response Object))
  case mResp of
    Nothing -> loggerPutStr logger "Bad response.\n" >> return Nothing
    Just resp ->
      let respBody = HM.insert "url" (String . fromString $ url) (resp L.^. R.responseBody)
          itemObject = Object respBody
          item = _parseMaybe itemObject (Proxy :: Proxy Item)
      in return $ case item of
                    Nothing -> Nothing
                    Just item' -> Just (item', itemObject)

parseKids :: Value -> [Kid]
parseKids valItem@(Object itemObject)
  | not (HM.member "kids" itemObject) = []
  | otherwise = maybe [] id $ _parseMaybe valItem (Proxy :: Proxy [Kid])

parseParts :: Value -> [Part]
parseParts valItem@(Object itemObject)
  | not (HM.member "parts" itemObject) = []
  | otherwise = maybe [] id $ _parseMaybe valItem (Proxy :: Proxy [Part])

_parseMaybe :: forall proxy a. (FromJSON a) => Value -> proxy a -> Maybe a
_parseMaybe val _ =
  case (parseEither parseJSON val) of
    Left e  -> Nothing
    Right v -> Just v

extendObjectWithModelId :: Key Item -> Value -> Value
extendObjectWithModelId dbItemKey (Object itemObject) =
  let (PersistInt64 keyVal) = head (keyToValues dbItemKey)
      valItem = Object $ HM.insert "model-id" (String . fromString . show $ keyVal) itemObject
  in valItem

_getItemId :: Object -> Parser (Key Item)
_getItemId obj = do
  (Right itemApiId) <- (\key -> keyFromValues [(PersistInt64 . read . fromString $ key)]) <$>
                        obj .: "model-id"
  return itemApiId

jsonToRawItem :: Object -> URL -> Maybe RawItem
jsonToRawItem itemObject url =
  let extendedObject = HM.insert "url" (String . fromString $ url) itemObject
      valItem = Object extendedObject
      item =  _parseMaybe valItem (Proxy :: Proxy Item)
  in case item of
       Nothing -> Nothing
       Just item' -> Just (item', valItem)

rawItemsFromFile :: FilePath -> IO (Maybe [RawItem])
rawItemsFromFile fp = do
  mValue <- decodeFileStrict fp :: IO (Maybe Value)
  return $ case mValue of
             Nothing -> Nothing
             Just (Array itemsArray) -> Just $ M.catMaybes
                                             . V.toList
                                             . V.map worker $ itemsArray
             Just _ -> Nothing
  where worker (Object itemObject) =
          let itemId = HM.lookup "id" itemObject
          in case itemId of
            Nothing -> Nothing
            Just itemId' ->
              let url = "https://hacker-news.firebaseio.com/v0/item/" ++ show itemId' ++ ".json"
              in jsonToRawItem itemObject url
        worker _ = Nothing

fullItemsFromFile :: FilePath -> DB (Maybe [FullItem])
fullItemsFromFile fp = do
  rawItems <- liftIO $ rawItemsFromFile fp
  case rawItems of
    Nothing -> return Nothing
    Just rawItems' -> Just . M.catMaybes <$> mapM worker rawItems'
  where worker :: RawItem -> DB (Maybe FullItem)
        worker (item, value) = do
          dbItemKey <- P.insertUnique item
          case dbItemKey of
            Nothing -> return Nothing
            Just key ->
              let extendedValue = extendObjectWithModelId key value
                  kids = parseKids extendedValue
                  parts = parseParts extendedValue
              in do mapM_ P.insertUnique kids >> mapM_ P.insertUnique parts
                    return . Just $ FullItem item kids parts
