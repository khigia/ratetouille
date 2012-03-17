module Handler.Collection where

import Import
import Yesod.Auth

import Handler.Entry (entryForm, getEntryListR, getEntryItemR)

collectionForm :: Form Collection
collectionForm = renderBootstrap $ Collection
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
    muser <- maybeAuth
    collections <- runDB $ selectList [] [Desc CollectionName]
    ((_, collectionWidget), enctype) <- generateFormPost collectionForm
    defaultLayout $ do
        setTitle "ratetouille collections"
        $(widgetFile "collections")

postCollectionItemR :: CollectionId -> Handler RepHtml
postCollectionItemR collectionId = do
    ((res, entryWidget), enctype) <- runFormPost (entryForm collectionId)
    case res of
        FormSuccess entry -> do
            _entryId <- runDB $ insert entry
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


