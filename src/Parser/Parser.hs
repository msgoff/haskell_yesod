{-# LANGUAGE TypeSynonymInstances #-}

module Parser.Parser where

import           Control.Monad.Trans.Class  (MonadTrans(..))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Control.Monad.Trans.Maybe  (MaybeT(..))
import           Data.IORef                 (IORef, modifyIORef)
import           Data.Text                  (Text)
import qualified Data.Aeson       as A
import qualified Network.HTTP.Req as R

import           Model (HNJob)
import           Parser.Types

type Job = HNJob
instance A.FromJSON Job where

type JobURL = Text
type JobsM a = MaybeT (ReaderT (IORef [Job]) IO) a

handleJob :: JobURL -> JobsM ()
handleJob url = do
  job <- lift . liftIO $ requestJob url
  undefined
--  lift $ storeJob job
  where storeJob job = do
          jobsContainerIORef <- ask
          liftIO $ modifyIORef jobsContainerIORef $ \jobs -> job : jobs

requestJob :: JobURL -> IO (Maybe Job) -- TODO
requestJob url = do
  resJson <- R.runReq httpConfig $ do
    res <- R.req R.GET (R.https url) R.NoReqBody R.jsonResponse mempty
    return (R.responseBody res :: A.Value)
  return (A.decode resJson :: Maybe Job)
  where httpConfig = R.HttpConfig
          { R.httpConfigProxy         = Nothing
          , R.httpConfigRedirectCount = 10
          , R.httpConfigAltManager    = Nothing
          , R.httpConfigCheckResponse = undefined -- TODO
          , R.httpConfigRetryPolicy   = undefined
          , R.httpConfigRetryJudge    = undefined
          } -- TODO
