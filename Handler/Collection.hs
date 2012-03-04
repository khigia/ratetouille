module Handler.Collection where

import Import

collectionForm :: Form Collection
collectionForm = renderDivs $ Collection
    <$> areq textField "name" Nothing
    <*> areq textField "stupid" Nothing

postCollectionsR :: Handler RepHtml
postCollectionsR = do
    ((res, collectionWidget), enctype) <- runFormPost collectionForm
    case res of
        FormSuccess collection -> do
            collectionId <- runDB $ insert collection
            setMessageI $ MsgCollectionCreated $ collectionName collection
            redirect $ CollectionR collectionId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectCollection
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{collectionWidget}
    <div>
        <input type=submit value="Create collection">
|]

getCollectionsR :: Handler RepHtml
getCollectionsR = do
    collections <- runDB $ selectList [] [Desc CollectionName]
    ((_, collectionWidget), enctype) <- generateFormPost collectionForm
    defaultLayout $ do
        setTitle "ratetouille collections"
        $(widgetFile "collections")

getCollectionR :: CollectionId -> Handler RepHtml
getCollectionR collectionId = do
    collection <- runDB $ do collection <- get404 collectionId
                             return collection
    defaultLayout $ do
        setTitle "ratetouille collection"
        $(widgetFile "collection")
