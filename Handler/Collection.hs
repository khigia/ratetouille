module Handler.Collection where

import Import

getCollectionR :: Handler RepHtml
getCollectionR = do
    defaultLayout $ do
        setTitle "ratetouille collections"
        $(widgetFile "collections")

