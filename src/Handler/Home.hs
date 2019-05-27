{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ExtendedDefaultRules         #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE FlexibleContexts             #-}

module Handler.Home where


import qualified Import    as I
import           Model                              (Item(..))

import           Text.Julius                        (RawJS (..))
import           System.Directory                   (doesFileExist)
import           Control.Concurrent                 (threadDelay, killThread, forkIO)
import           Data.Proxy                         (Proxy(..))
import           Data.Data                          (Data(..), constrFields, dataTypeConstrs)
import           Data.Text                          (Text)
import qualified Data.Text as T                     (replace, pack, toLower)
import           Data.Aeson
import           Data.Maybe                         (catMaybes, fromJust)
import qualified Data.Map            as M           (fromAscListWith, toList)
import           Data.Map                           (Map, (!))
import qualified Data.HashMap.Strict as HM          (toList)

import           Database.Esqueleto

import           Parser.Parser

default (Text)

getPullR :: Int -> Int -> Int -> I.Handler I.Html
getPullR amount interval stop = do
  localData <- I.appLocalData <$> I.getYesod
  jsonExists <- I.liftIO $ or <$> mapM doesFileExist localData
  if jsonExists
    then I.runDB $ mapM_ fullItemsFromFile localData
    else return ()
  _ <- runMonitor amount interval (Just stop)
  I.defaultLayout [I.whamlet| |]

getHomeR :: I.Handler I.Html
getHomeR = do
    app <- I.getYesod
    items <- toFullItems <$> I.runDB itemsFromDB
    let itemsData = juliusCombineByComma (makeItemsData items)
        itemColumnDefinitions = juliusCombineByComma [ columnDefinitions
                                                     , mkDefList "Kids"
                                                     , mkDefList "Parts" ]
    I.defaultLayout $ do
        I.setTitle "Welcome To Yesod!"
        $(I.widgetFile "home/home")
    where juliusCombineByComma = foldr (\a b -> a <> [I.julius| , |] <> b) mempty

          mkDef hdr = [I.julius| { headerName: #{hdr}, field: #{T.toLower hdr} } |]
          mkDefWithRenderer hdr cellRenderer =
            [I.julius| { headerName: #{hdr},
                         field: #{T.toLower hdr},
                         cellRenderer: #{cellRenderer} } |]
          mkDefList = flip mkDefWithRenderer "listCellRenderer"

          makeItemsData [] = []
          makeItemsData (FullItem item kids parts:xs) =
            let (Object valItem) = toJSON item
                columns = buildListField "kids" (map I.kidApiId kids)
                        : buildListField "parts" (map I.partApiId parts)
                        : (flip map (HM.toList valItem) $ \(field, value) ->
                            let field' = T.toLower . T.replace "item" "" $ field
                            in case value of
                                 String a -> [I.julius| #{field'}: #{a} |]
                                 Bool a -> [I.julius| #{field'}: #{show a} |]
                                 Number a -> [I.julius| #{field'}: #{show a} |]
                                 _ -> [I.julius| #{field}: "" |])
                row      = [I.julius| { |] <> juliusCombineByComma columns <> [I.julius| } |]
            in row : makeItemsData xs
            where buildListField field xs = let listJS = rawJS (show xs)
                                            in [I.julius| #{field}: #{listJS} |]

          columnDefinitions =
            let item = dataTypeOf (undefined :: Item)
                itemFields = constrFields . head . dataTypeConstrs $ item
                itemPrefix = "item"
                noPrefixFields = map (T.replace itemPrefix "" . T.pack) itemFields
            in juliusCombineByComma (map mkDef noPrefixFields)

type ItemMap a b = Map (Entity Item) [Entity b]

-- FIXME: Naive implementation. Refactoring is needed.
toFullItems :: (ItemMap Item I.Kid, ItemMap Item I.Part) -> [FullItem]
toFullItems (a, b) = worker (M.toList a) (M.toList b)
  where worker [] _ = []
        worker ((item, kids):xs) partsMap = flip (:) (worker xs partsMap) $
          FullItem (entityVal item) (map entityVal kids) $
            (map entityVal . fromJust $ lookup item partsMap)

itemsFromDB :: I.SqlPersistT I.Handler (ItemMap Item I.Kid, ItemMap Item I.Part)
itemsFromDB = worker I.KidItemId >>= \a -> worker I.PartItemId >>= \b -> return (a, b)
  where worker field = groupByItem <$>
          (select $
           from $ \(item `LeftOuterJoin` m) -> do
           on (just (item ^. I.ItemId) ==. m ?. field)
           return (item, m))
        groupByItem l = M.fromAscListWith (++) $ flip map l $ \(a, b) -> (a, catMaybes [b])
