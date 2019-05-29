{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ExtendedDefaultRules         #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE FlexibleContexts             #-}

module Handler.Home where


import qualified Import    as I
import           Model                              (Item(..))

import           Text.Julius                        (RawJS (..), renderJavascriptUrl)
import           System.Directory                   (doesFileExist)
import qualified Control.Concurrent.Async   as A    (mapConcurrently_)
import           Data.Data                          (Data(..), constrFields, dataTypeConstrs)
import           Data.Text.Lazy                     (Text)
import qualified Data.Text as T                     (replace, pack, unpack, toLower)
import           Data.Aeson          as A
import           Data.Maybe                         (catMaybes, fromJust)
import qualified Data.Map            as M           (fromAscListWith, toList)
import           Data.Map                           (Map)
import qualified Data.HashMap.Strict as HM          (toList)

import           Database.Esqueleto

import           Parser.Parser

default (Text)

getPullLocalR :: I.Handler I.Html
getPullLocalR = do
  localData <- I.appLocalData <$> I.getYesod
  jsonExists <- I.liftIO $ or <$> mapM doesFileExist localData
  runInnerHandler <- I.handlerToIO
  if jsonExists
    then I.liftIO $ A.mapConcurrently_ (runInnerHandler . I.runDB . fullItemsFromFile) localData
    else return ()
  I.defaultLayout [I.whamlet| |]

getPullR :: Int -> Int -> Int -> I.Handler I.Html
getPullR amount interval stop = do
  _ <- runMonitor amount interval (Just stop)
  getPullLocalR

juliusCombineByComma = foldr (\a b -> a <> [I.julius| , |] <> b) mempty

postGetRowsR :: I.Handler Text
postGetRowsR = do
  (Just startRow) <- I.lookupPostParam "startRow"
  (Just endRow) <- I.lookupPostParam "endRow"
  items <- fmap toFullItems . I.runDB $ itemsFromDB (read . T.unpack $ startRow) (read . T.unpack $ endRow)
  let itemsData = [I.julius| [ |] <> juliusCombineByComma (makeItemsData items) <> [I.julius| ] |]
  let js = renderJavascriptUrl (\_ _ -> undefined) itemsData
  return js
  where makeItemsData [] = []
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

getHomeR :: I.Handler I.Html
getHomeR =
    let itemColumnDefinitions = juliusCombineByComma [ columnDefinitions
                                                     , mkDefList "Kids"
                                                     , mkDefList "Parts" ]
    in I.defaultLayout $ do
         I.setTitle "Welcome To Yesod!"
         $(I.widgetFile "home/home")
    where mkDef hdr = [I.julius| { headerName: #{hdr}, field: #{T.toLower hdr} } |]
          mkDefWithRenderer hdr cellRenderer =
            [I.julius| { headerName: #{hdr},
                         field: #{T.toLower hdr},
                         cellRenderer: #{cellRenderer} } |]
          mkDefList = flip mkDefWithRenderer "listCellRenderer"

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

itemsFromDB :: I.Int64 -> I.Int64 -> I.SqlPersistT I.Handler (ItemMap Item I.Kid, ItemMap Item I.Part)
itemsFromDB s e = go s e I.KidItemId >>= \a -> go s e I.PartItemId >>= \b -> return (a, b)
  where go startRow endRow field = groupByItem <$>
          ( select $
              from $ \(item `LeftOuterJoin` m) -> do
                let itemsSlice = subList_select $
                                   from $ \item' -> do
                                   orderBy [asc (item' ^. I.ItemApiId)]
                                   limit (endRow - startRow)
                                   offset startRow
                                   return (item' ^. I.ItemId)
                on (just (item ^. I.ItemId) ==. m ?. field)
                where_ (item ^. I.ItemId `in_` itemsSlice)
                orderBy [asc (item ^. I.ItemApiId)]
                return (item, m) )
        groupByItem l = M.fromAscListWith (++) $ flip map l $ \(a, b) -> (a, catMaybes [b])
