module Handler.Entry where

import Import

entryForm :: CollectionId -> Maybe Entry -> Form Entry
entryForm collectionId mentry =
    renderBootstrap $ Entry
    <$> pure collectionId
    <*> areq textField "text" (entryText <$> mentry)


getEntryListR :: Handler RepHtml
getEntryListR = do
    entries <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "ratetouille entries"
        $(widgetFile "entries")

getEntryItemR :: EntryId -> Handler RepHtml
getEntryItemR entryId = do
    entry <- runDB $ do get404 entryId
    ((_, entryWidget), enctype) <- generateFormPost (entryForm (entryCollectionId entry) (Just entry))
    defaultLayout $ do
        setTitle "ratetouille entry"
        $(widgetFile "entry")

postEntryItemR :: EntryId -> Handler RepHtml
postEntryItemR entryId = do
    oldEntry <- runDB $ do get404 entryId
    let collectionId = entryCollectionId oldEntry
    ((res, _entryWidget), _enctype) <- runFormPost (entryForm collectionId Nothing)
    case res of
        FormSuccess entry -> do
            runDB $ replace entryId entry
            setMessage "Entry modified"
            redirect $ CollectionItemR (entryCollectionId entry)
        _ -> do
            setMessage "Please correct entry"
            redirect $ EntryItemR entryId

deleteEntry entryId = do
    deleteWhere [RatingEntryId ==. entryId]
    delete entryId

getEntryItemDeleteR :: EntryId -> Handler RepHtml
getEntryItemDeleteR entryId = do
    entry <- runDB $ get404 entryId
    _ <- runDB $ deleteEntry entryId
    let collectionId = entryCollectionId entry
    do
        setMessage "Entry (and ratings) deleted"
        redirect $ CollectionItemR collectionId
