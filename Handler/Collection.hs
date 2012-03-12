module Handler.Collection where

import Import
import Yesod.Auth
import Yesod.Form.MassInput
import Control.Arrow
import Control.Monad (liftM)
import Data.Text (pack)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequenceA)

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

entryForm :: CollectionId -> Form Entry
entryForm collectionId = renderBootstrap $ Entry
    <$> pure collectionId
    <*> areq textField "text" Nothing

data Entrat = Entrat {
    erEntry::Entry
  , erRating::Maybe Rating
}

getCollectionItemR :: CollectionId -> Handler RepHtml
getCollectionItemR collectionId = do
    muser <- maybeAuth
    (collection, entries) <- runDB $ do
        collection <- get404 collectionId
        entries <- selectList [EntryCollectionId ==. collectionId] []
        return (collection, entries)
    let getRatings Nothing _ = runDB $ return []
        getRatings (Just (Entity userId _)) es = runDB $
          selectList [ RatingUserId ==. userId,
                       RatingEntryId <-. eids es] []
        eids = map (\(Entity eid _) -> eid)
    ratings <- getRatings muser entries
    -- painfully doing a join ...
    let cmpK ka kb | ka <= kb = LT
                   | otherwise = GT
        cmpE (Entity ka _) (Entity kb _) = cmpK ka kb
        cmpR (Entity _ a) (Entity _ b) = cmpK (ratingEntryId a) (ratingEntryId b)
        sortedEntries = sortBy cmpE entries
        sortedRatings = sortBy cmpR ratings
        ziprat []     _      result = result
        ziprat (e:es) []     result = ziprat es [] ((Entrat (valE e) Nothing):result)
        ziprat (e:es) (r:rs) result =
            if keyE e == keyR r
            then ziprat es rs ((Entrat (valE e) (Just $ valR r)):result)
            else ziprat es (r:rs) ((Entrat (valE e) Nothing):result)
        keyE (Entity k _) = k
        valE (Entity _ v) = v
        keyR (Entity _ x) = ratingEntryId x
        valR (Entity _ x) = x
    let entrat = ziprat sortedEntries sortedRatings []
    let Just (Entity userId _) = muser
    let Entity entryId _ = head entries
    let Entity entryId2 _ = head $ tail entries
    ((_, ratingWidget), renctype) <- generateFormPost $ renderDivs (ratingAForm userId entryId Nothing)
    ((_, ratingWidget2), renctype2) <- generateFormPost $ renderDivs (ratingAForm userId entryId2 Nothing)
    ((_, entryWidget), enctype) <- generateFormPost (entryForm collectionId)
    --((resRatingsWidget, ratingsWidget), ratingsEnctype) <- generateFormPost $ renderDivs (ratingMassAForm userId entryId (Just $ map (\(Entity k v) -> v) sortedRatings)) -- do not pass entryId? at least list of!
    ((resRatingsWidget, ratingsWidget), ratingsEnctype) <- generateFormPost $ renderBootstrap (ratingMassAForm userId entryId (Just [Rating userId entryId 3, Rating userId entryId 2])) -- do not pass entryId? at least list of!
    defaultLayout $ do
        setTitle "ratetouille collection"
        $(widgetFile "collection")


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

ratingAForm :: UserId -> EntryId -> Maybe Rating -> AForm Rat Rat Rating
ratingAForm userId entryId mrating = Rating
    <$> pure userId
    <*> pure entryId
    <*> areq (radioFieldList rates) "value" (ratingValue <$> mrating)
  where
    rates = map (pack . show &&& id) $ [1..5]

ratingMassAForm userId entryId mratings = inputList "Ratings" massDivs
    (\x -> Rating
        <$> pure userId
        <*> pure entryId
        <*> areq (radioFieldList rates) "value" (ratingValue <$> x)) mratings
  where
    rates = map (pack . show &&& id) $ [1..5]


testFormSingle :: MForm Rat Rat (FormResult Collection, Widget)
testFormSingle = do
    (nameRes, nameView) <- mreq textField "unsued" Nothing
    (stupidRes, stupidView) <- mreq textField "unused" Nothing
    let allRes = Collection <$> nameRes <*> stupidRes
    let widget = do
        toWidget [whamlet|
<p>
    Some collection with stupid #
    ^{fvInput stupidView}
    \ and name #
    ^{fvInput nameView}
    \please!
|]
    return (allRes, widget)

testFormSingle' :: Maybe Collection -> MForm Rat Rat (FormResult Collection, Widget)
testFormSingle' mcol = do
    (nameRes, nameView) <- mreq textField "unsued" (collectionName <$> mcol)
    (stupidRes, stupidView) <- mreq textField "unused" (collectionStupid <$> mcol)
    let allRes = Collection <$> nameRes <*> stupidRes
    let widget = do
        toWidget [whamlet|
<p>
    Some collection with stupid #
    ^{fvInput stupidView}
    \ and name #
    ^{fvInput nameView}
    \please!
|]
    return (allRes, widget)

testForm :: Maybe [Collection] -> Html -> MForm Rat Rat (FormResult [Collection], Widget)
testForm mvals extra = do
    let vals = map Just $ fromMaybe [] mvals
    (res, wids) <- liftM fixme $ mapM testFormSingle' vals
    let widget = do
        toWidget [whamlet|
 #{extra}
<p>
    $forall wid <- wids
        ^{wid}
    <input type=submit value="Go!">
|]
    return (res, widget)
  where
    fixme vs = (res, w)
      where res = sequenceA $ map fst vs
            w   = map snd vs


getTestR :: Handler RepHtml
getTestR = do
    ((res, widget), enctype) <- runFormGet $ testForm $ Just [Collection "a" "b", Collection "c" "d"]
    defaultLayout [whamlet|
<p>Result: #{show res}
<form enctype=#{enctype}>
    ^{widget}
|]
