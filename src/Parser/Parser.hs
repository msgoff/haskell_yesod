{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}

module Parser.Parser where

import           Control.Monad                      (forever)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Reader         (ReaderT)
import           Control.Exception                  (catch)
import           Control.Concurrent                 (threadDelay)
import           Data.IORef                         (newIORef, readIORef, writeIORef)
import qualified Data.Text                  as T    (unpack, pack, splitOn)
import           Data.Text                          (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL  (unpack)
import qualified Data.Set                   as S
import           Data.Set                           (Set)
import qualified Data.Time.Clock.POSIX      as Time (posixSecondsToUTCTime)

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types                   (Value(..), parseMaybe)
import qualified Network.Wreq               as R
import           Network.Wreq                       (Response, JSONError(..))

import           Data.Pool                          (Pool)
import qualified Database.Persist.Class     as P    (insertUnique)
import qualified Database.Persist.Sql       as P    (runSqlPool)
import           Database.Persist.Sql               (SqlBackend)

import           Model                      hiding  (JobId)
import           Parser.Types

default (Text)

type JobURL = String
type JobId  = Text

monitorJobs :: Interval -> Pool SqlBackend -> IO ()
monitorJobs interval connPool =
  let baseUrl        = "https://hacker-news.firebaseio.com/v0/jobstories.json"
  in do
    storedIdsIORef <- newIORef S.empty
    forever $ do
      storedIds <- readIORef storedIdsIORef
      jobIds <- receiveJobIds baseUrl
      let newJobIds = S.difference (S.fromList jobIds) storedIds
      receivedJobIds <- flip P.runSqlPool connPool $ worker (S.toList newJobIds)
      writeIORef storedIdsIORef (S.union storedIds receivedJobIds)
      threadDelay (interval * 1000000)
  where worker :: [JobId] -> ReaderT SqlBackend IO (Set JobId)
        worker [] = return S.empty
        worker (jobId:xs) =
          let url = "https://hacker-news.firebaseio.com/v0/item/" ++ (T.unpack jobId) ++ ".json"
          in do job <- liftIO $ parseJob url
                case job of
                  Nothing -> worker xs
                  Just job' -> do
                    P.insertUnique job'
                    rest <- worker xs
                    return $ S.insert jobId rest

receiveJobIds :: String -> IO [JobId]
receiveJobIds url = do
  resp <- R.get url
  let respBody = BSL.unpack (resp ^. R.responseBody)
  let jobIds   = T.splitOn "," (T.pack . tail . init $ respBody)
  -- FIXME: That is not a safe way to decode a json.
  --        JSON must be parsed to a list of strings with Aeson.
  return jobIds

instance FromJSON (JobURL -> Job) where
  parseJSON (Object obj) = do
    jobUsername <- obj .: "by"
    jobScore <- obj .: "score"
    jobCreated <- toUTC <$> obj .: "time"
    jobTitle <- obj .: "title"
    jobDescription <- obj .: "text"
    return $ \jobUrl' -> let jobUrl = T.pack jobUrl' in Job{..}
    where toUTC = Time.posixSecondsToUTCTime
  parseJSON _ = error "Not an object."

parseJob :: JobURL -> IO (Maybe Job)
parseJob url = do
  mResp <- flip catch (\(JSONError _) -> return Nothing) $
             Just <$> (R.asJSON =<< R.get url :: IO (Response Object))
  return $
    case mResp of
      Nothing -> Nothing
      Just resp -> let respBody = resp ^. R.responseBody
                       toJob    = parseMaybe parseJSON (Object respBody)
                   in case toJob of
                        Nothing -> Nothing
                        Just f -> Just (f url)
