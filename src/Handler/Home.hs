{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeFamilies                 #-}

module Handler.Home where


import qualified Import    as I
import           Model          (Item(..))

import           Data.Data      (Data(..), constrFields, dataTypeConstrs)
import qualified Data.Text as T (replace, pack, toLower)

import           Parser.Parser  (discoverItems)


getHomeR :: I.Handler I.Html
getHomeR = do
    app <- I.getYesod
    items <- I.runDB $ discoverItems (I.appLogger app) 20
    I.defaultLayout $ do
        I.setTitle "Welcome To Yesod!"
        $(I.widgetFile "home/home")
    where columnDefinitions =
            let item = dataTypeOf (undefined :: Item)
                itemFields = constrFields . head . dataTypeConstrs $ item
                itemPrefix = "item"
                noPrefixFields = map (T.replace itemPrefix "" . T.pack) itemFields
            in foldr (\a b -> a <> [I.julius| , |] <> b) mempty (map mkDef noPrefixFields)
            where mkDef hdr = [I.julius| { headerName: #{hdr}, field: #{T.toLower hdr} } |]
