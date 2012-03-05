module Handler.Collection where

import Import

collectionForm :: Form Collection
collectionForm = renderDivs $ Collection
    <$> areq textField "name" Nothing
    <*> areq textField "stupid" Nothing

postCollectionListR :: Handler RepHtml
postCollectionListR = do
    ((res, collectionWidget), enctype) <- runFormPost collectionForm
    case res of
        FormSuccess collection -> do
            collectionId <- runDB $ insert collection
            setMessageI $ MsgCollectionCreated $ collectionName collection
            redirect $ CollectionItemR collectionId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectCollection
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{collectionWidget}
    <div>
        <input type=submit value="Create collection">
|]

getCollectionListR :: Handler RepHtml
getCollectionListR = do
    collections <- runDB $ selectList [] [Desc CollectionName]
    ((_, collectionWidget), enctype) <- generateFormPost collectionForm
    defaultLayout $ do
        setTitle "ratetouille collections"
        $(widgetFile "collections")

entryForm :: CollectionId -> Form Entry
entryForm collectionId = renderDivs $ Entry
    <$> pure collectionId
    <*> areq textField "text" Nothing

getCollectionItemR :: CollectionId -> Handler RepHtml
getCollectionItemR collectionId = do
    (collection, entries) <- runDB $ do
        collection <- get404 collectionId
        entries <- selectList [EntryCollectionId ==. collectionId] []
        return (collection, entries)
    ((_, entryWidget), enctype) <- generateFormPost (entryForm collectionId)
    defaultLayout $ do
        setTitle "ratetouille collection"
        $(widgetFile "collection")

postCollectionItemR :: CollectionId -> Handler RepHtml
postCollectionItemR collectionId = do
    ((res, entryWidget), enctype) <- runFormPost (entryForm collectionId)
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessage "Entry created"
            redirect $ CollectionItemR collectionId
        _ -> defaultLayout $ do
            setTitle "Please correct entry"
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value="Create entry">
|]

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
