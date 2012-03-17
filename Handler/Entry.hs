module Handler.Entry where

import Import

entryForm :: CollectionId -> Form Entry
entryForm collectionId = renderBootstrap $ Entry
    <$> pure collectionId
    <*> areq textField "text" Nothing


getEntryListR :: Handler RepHtml
getEntryListR = do
    entries <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "ratetouille entries"
        $(widgetFile "entries")

getEntryItemR :: EntryId -> Handler RepHtml
getEntryItemR entryId = do
    entry <- runDB $ do get404 entryId
    defaultLayout $ do
        setTitle "ratetouille entry"
        $(widgetFile "entry")

