{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE FlexibleContexts             #-}

module Handler.Home where


import qualified Import    as I
import           Model                              (Item(..))

import           Control.Concurrent                 (threadDelay, killThread)
import           Data.Data                          (Data(..), constrFields, dataTypeConstrs)
import qualified Data.Text as T                     (replace, pack, toLower)
import           Data.Aeson
import           Data.Maybe                         (catMaybes, fromJust)
import qualified Data.Map            as M           (fromAscListWith, toList)
import           Data.Map                           (Map, (!))
import qualified Data.HashMap.Strict as HM          (toList)

import           Database.Esqueleto

import           Parser.Parser                      (runMonitor, discoverItems)


getHomeR :: I.Handler I.Html
getHomeR = do
    app <- I.getYesod
    -- _ <- runMonitor 20 5 (Just 100)
    --eitems <- toExtendedItems <$> I.runDB itemsFromDB
    --I.liftIO $ flip mapM_ eitems (\g -> print g >> putStrLn "\n\n")
    items <- (I.runDB $ select $
                        from $ \item -> do
                        return item) :: I.Handler [Entity Item]
    let itemsData = juliusCombineByComma (makeItemsData (map entityVal items))
    I.defaultLayout $ do
        I.setTitle "Welcome To Yesod!"
        $(I.widgetFile "home/home")
    where juliusCombineByComma = foldr (\a b -> a <> [I.julius| , |] <> b) mempty

          makeItemsData [] = []
          makeItemsData (item:xs) =
            let (Object valItem) = toJSON item
                columns = flip map (HM.toList valItem) $ \(field, value) ->
                            let field' = T.toLower . T.replace "item" "" $ field
                            in case value of
                                 String a -> [I.julius| #{field'}: #{a} |]
                                 Bool a -> [I.julius| #{field'}: #{show a} |]
                                 Number a -> [I.julius| #{field'}: #{show a} |]
                                 _ -> [I.julius| #{field}: "" |]
                row      = [I.julius| { |] <> juliusCombineByComma columns <> [I.julius| } |]
            in row : makeItemsData xs

          columnDefinitions =
            let item = dataTypeOf (undefined :: Item)
                itemFields = constrFields . head . dataTypeConstrs $ item
                itemPrefix = "item"
                noPrefixFields = map (T.replace itemPrefix "" . T.pack) itemFields
            in juliusCombineByComma (map mkDef noPrefixFields)
            where mkDef hdr = [I.julius| { headerName: #{hdr}, field: #{T.toLower hdr} } |]

type ItemMap a b = Map (Entity Item) [Entity b]

data ExtendedItem = ExtendedItem Item [I.Kid] [I.Part]
  deriving (Show)

-- FIXME: Naive implementation. Refactoring is needed.
toExtendedItems :: (ItemMap Item I.Kid, ItemMap Item I.Part) -> [ExtendedItem]
toExtendedItems (a, b) = worker (M.toList a) (M.toList b)
  where worker [] _ = []
        worker ((item, kids):xs) partsMap = flip (:) (worker xs partsMap) $
          ExtendedItem (entityVal item) (map entityVal kids) $
            (map entityVal . fromJust $ lookup item partsMap)

itemsFromDB :: I.SqlPersistT I.Handler (ItemMap Item I.Kid, ItemMap Item I.Part)
itemsFromDB = worker I.KidItemId >>= \a -> worker I.PartItemId >>= \b -> return (a, b)
  where worker field = groupByItem <$>
          (select $
           from $ \(item `LeftOuterJoin` m) -> do
           on (just (item ^. I.ItemId) ==. m ?. field)
           return (item, m))
        groupByItem l = M.fromAscListWith (++) $ flip map l $ \(a, b) -> (a, catMaybes [b])
