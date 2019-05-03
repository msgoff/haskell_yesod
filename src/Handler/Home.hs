{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import           Import
import           Text.Julius    (RawJS (..))
import           Model          (Item(..))

import           Data.Data      (constrFields, toConstr)
import qualified Data.Text as T (replace, pack)

import           Parser.Parser  (discoverItems)

getHomeR :: Handler Html
getHomeR = do
    app <- getYesod
    items <- runDB $ discoverItems (appLogger app) 5
    liftIO $ print items
    columnDefinitions <- liftIO $ buildColumnDefs items
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "home/home")
    where buildColumnDefs [] = return []
          buildColumnDefs (item:xs) =
            let itemFields = constrFields . toConstr $ item
                itemPrefix = "item"
                noPrefixFields = map (T.replace itemPrefix "" . T.pack) itemFields
            in print item >> buildColumnDefs xs
