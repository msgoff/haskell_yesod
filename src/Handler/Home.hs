{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeFamilies                 #-}

module Handler.Home where


import qualified Import    as I
import           Model                              (Item(..))

import           Control.Monad                      (forever)
import           Control.Monad.Coroutine            (Coroutine(..))
import Control.Monad.Coroutine.SuspensionFunctors   (Yield(..))
import           Data.IORef                         (newIORef)
import           Data.Data                          (Data(..), constrFields, dataTypeConstrs)
import qualified Data.Text as T                     (replace, pack, toLower)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM          (toList)

import           Parser.Parser                      (mkDiscoverStep)


getHomeR :: I.Handler I.Html
getHomeR = do
    app <- I.getYesod
    Left (Yield items cont) <- resume $ mkDiscoverStep (I.appLogger app) 20
    let itemsData = juliusCombineByComma (makeItemsData items)
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
