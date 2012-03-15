module Handler.Collection where

import Import
import Yesod.Auth
--import Yesod.Form.MassInput

import Control.Applicative (liftA)
import Control.Arrow
import Control.Monad (liftM)
import Control.Monad.Trans.RWS (ask)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (pack, unpack)
import Data.Text.Read (decimal)
import Data.Traversable (sequenceA)
import qualified Data.Map as Map

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
    -- TODO get the EntryStats from DB (sum of votes)
    -- Get the user ratings
    let getRatings Nothing _ = runDB $ return []
        getRatings (Just (Entity userId _)) es = runDB $
          selectList [ RatingUserId ==. userId,
                       RatingEntryId <-. eids es] []
        eids = map (\(Entity eid _) -> eid)
    ratings <- getRatings muser entries
    -- Painfully doing a join Entries with user's Ratings
    let cmpK ka kb | ka <= kb = LT
                   | otherwise = GT
        cmpE (Entity ka _) (Entity kb _) = cmpK ka kb
        cmpR (Entity _ a) (Entity _ b) = cmpK (ratingEntryId a) (ratingEntryId b)
        sortedEntries = sortBy cmpE entries
        sortedRatings = sortBy cmpR ratings
        ziprat []     _      result = result
        ziprat (e:es) []     result = ziprat es [] ((e, Nothing):result)
        ziprat (e:es) (r:rs) result =
            if keyE e == keyR r
            then ziprat es rs ((e, Just $ valR r):result)
            else ziprat es (r:rs) ((e, Nothing):result)
        keyE (Entity k _) = k
        valE (Entity _ v) = v
        keyR (Entity _ x) = ratingEntryId x
        valR (Entity _ x) = x
    let entrat = ziprat sortedEntries sortedRatings []
    -- Widget to create new Entry
    ((_, entryWidget), enctype) <- generateFormPost (entryForm collectionId)
    -- Widget for user to rate entries
    let Just (Entity userId userVal) = muser -- TODO user maybe!
    ((ratingsRes, ratingsWidget), ratingsEnctype) <- generateFormPost $ ratingsMForm userId (Just (map Just entrat))
    defaultLayout $ do
        setTitle "ratetouille collection"
        --addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        addScriptRemote "/static/js/jquery-ui-1.8.18.custom/js/jquery-1.7.1.min.js"
        addScriptRemote "/static/js/jquery-ui-1.8.18.custom/js/jquery-ui-1.8.18.custom.min.js"
        addScriptRemote "/static/js/jquery.ui.stars-3.0/jquery.ui.stars.js"
        toWidgetHead [hamlet|
<link rel="stylesheet" type="text/css" href="/static/js/jquery.ui.stars-3.0/jquery.ui.stars.css">
|]
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


ratingMForm :: UserId
            -> Maybe (Entity Entry, Maybe Rating)
            -> MForm Rat Rat (FormResult Rating, Widget)
ratingMForm userId mentrat = do
    let rates = map (pack . show &&& id) [1..3]
    -- TODO probalbly unKey, fromPersistValue toPersistValue are better
    -- than show/read
    (entryRes, entryView) <- mreq hiddenField "unsued" ((pack . show . entityKey . fst) <$> mentrat)
    let mrating = fromMaybe Nothing $ liftM snd mentrat
    -- TODO make this field hidden, create own radio view acting on
    -- the hidden field through jquery ui stars
    (valueRes, valueView) <- mopt (radioFieldList rates) (FieldSettings {fsLabel=pack "unsued", fsTooltip=Nothing, fsId=Nothing, fsName=Nothing, fsClass=[]}) (ratingValue <$> mrating)
    let res = Rating <$> pure userId
                     <*> liftA (read . unpack) entryRes
                     <*> valueRes
    let widget = do
        toWidget [whamlet|
<li>
    $maybe entrat <- mentrat
        #{entryText $ entityVal $ fst entrat}
    ^{fvInput entryView}
    ^{fvInput valueView}
|]
    return (res, widget)

ratingsMForm :: UserId
             -> Maybe [Maybe (Entity Entry, Maybe Rating)]
             -> Html
             -> MForm Rat Rat (FormResult [Rating], Widget)
ratingsMForm userId mentrats extra = do
    countName <- newFormIdent
    (menv, _, _) <- ask
    let readInt t =
            case decimal t of
                Right (i, "") -> Just i
                _ -> Nothing
    let vals =
            case menv of
                Nothing -> fromMaybe [] mentrats
                Just (env, _) ->
                    let c = fromMaybe 0 $ Map.lookup countName env >>= listToMaybe >>= readInt
                     in replicate c Nothing
    -- TODO if env has entryCount=N param, use a list of N Nothing
    -- instead of mentrats
    -- TODO read env to extract either of:
    --   all entryId stored as hiddenField
    --   an hidden field which is the count of entryId
    -- else can query all entries of collection and hope collection
    -- hasn't changed :(
    -- TODO initialize with a list of N Nothing, N being read in env,
    -- and we can even load the entries if we want
    let aggr rws = (sequenceA $ map fst rws, map snd rws)
    (res, wids) <- liftM aggr $ mapM (ratingMForm userId) vals
    let valCount = length vals

    let widget = do -- TODO in template file?
        toWidget [whamlet|
 #{extra}
<ul>
    $forall wid <- wids
        ^{wid}
<input .count type=hidden name=#{countName} value=#{valCount}>
|]
        toWidget [julius|
$("#stars-wrapper1").stars();
$("#ratingsform label").hide();
$("#ratingsform input[value=none]").hide();
|]
    return (res, widget)

postRatingListR :: CollectionId -> Handler RepHtml
postRatingListR collectionId = do
    muser <- maybeAuth
    let Just (Entity userId _) = muser -- TODO user maybe!
    ((res, _widget), _enctype) <- runFormPost (ratingsMForm userId Nothing)
    setMessage $ toHtml $ show res
    -- save rating in DB
    case res of
        FormSuccess ratings -> do
            let insertOrUpdate r = do
                  erating <- runDB $ insertBy r
                  case erating of
                      Left entity -> runDB $ replace (entityKey entity) r
                      Right _k -> return ()
            mapM_ insertOrUpdate ratings
            redirect $ CollectionItemR collectionId
                --erating <- runDB $ insertBy rating
        _ -> redirect $ CollectionItemR collectionId
    -- TODO update EntryStats and save
