{- | The custom card marketplace: sets an author has published, and other
people's copies following them.

Publishing snapshots the set's cards into a new version. Subscribing runs the
same import a file import does, and records which version it landed on, so the
copy can be brought up to date later. Editing that copy deletes the record --
what is in it is then not what was published -- which is the whole of the
"subscribed or not" rule.
-}
module Api.Handler.Arkham.PublishedCardSets (
  getApiV1ArkhamPublishedCardSetsR,
  getApiV1ArkhamPublishedCardSetR,
  deleteApiV1ArkhamPublishedCardSetR,
  postApiV1ArkhamPublishedCardSetSubscribeR,
  postApiV1ArkhamCustomCardSetPublishR,
  postApiV1ArkhamCustomCardSetSyncR,
  postApiV1ArkhamPublishedCardSetLikeR,
  deleteApiV1ArkhamPublishedCardSetLikeR,
) where

import Api.Handler.Arkham.CustomCards (ownedCardSet, persistCard, prepareCardForSet)
import Arkham.Card.CustomCard (CustomCard (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Char (isDigit)
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist qualified as DB
import Import hiding ((==.))
import Import qualified as P
import Json hiding (Success)
import Network.HTTP.Types.Status qualified as Status

-- | A marketplace row: what it is, who wrote it, and where you stand with it.
data PublishedCardSetResponse = PublishedCardSetResponse
  { publishedCardSetResponseId :: ArkhamPublishedCardSetId
  , publishedCardSetResponseName :: Text
  , publishedCardSetResponseAuthor :: Text
  , publishedCardSetResponseMine :: Bool
  , publishedCardSetResponseLatestVersion :: Int
  , publishedCardSetResponseCardCount :: Int
  , publishedCardSetResponseNote :: Maybe Text
  , -- | The first few cards, so the listing can show what is in the set rather
    -- than only how much. Capped because a listing is a row, not the set.
    publishedCardSetResponsePreview :: [CustomCard]
  , publishedCardSetResponseLikes :: Int
  , -- | Whether you are one of them, so the button knows which way it points.
    publishedCardSetResponseLiked :: Bool
  , publishedCardSetResponseUpdatedAt :: UTCTime
  , -- | The version your own copy is following, if you have one.
    publishedCardSetResponseSubscribedVersion :: Maybe Int
  }
  deriving stock Generic

instance ToJSON PublishedCardSetResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "publishedCardSetResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "publishedCardSetResponse"

{- | One set, in full: its listing and every card in the version being looked at.

The listing comes along so a page showing one set needs one request rather than
this plus a sweep of the whole marketplace to find the row it belongs to.
-}
data PublishedCardSetVersionResponse = PublishedCardSetVersionResponse
  { publishedCardSetVersionResponseListing :: PublishedCardSetResponse
  , publishedCardSetVersionResponseVersion :: Int
  , publishedCardSetVersionResponseName :: Text
  , publishedCardSetVersionResponseNote :: Maybe Text
  , publishedCardSetVersionResponseCards :: [CustomCard]
  , publishedCardSetVersionResponseCreatedAt :: UTCTime
  }
  deriving stock Generic

instance ToJSON PublishedCardSetVersionResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "publishedCardSetVersionResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "publishedCardSetVersionResponse"

newtype PublishPost = PublishPost {publishNote :: Maybe Text}
  deriving stock Generic

instance FromJSON PublishPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "publish"

-- | Which version to take. Absent means the newest.
newtype SubscribePost = SubscribePost {subscribeVersion :: Maybe Int}
  deriving stock Generic

instance FromJSON SubscribePost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "subscribe"

-- | The cards of a version, or a 500 that says which version could not be read.
versionCards :: ArkhamPublishedCardSetVersion -> Handler [CustomCard]
versionCards row =
  case parseEither parseJSON (arkhamPublishedCardSetVersionCards row) of
    Right cards -> pure cards
    Left err ->
      sendResponseStatus Status.status500
        $ object ["error" .= ("Could not read the published cards: " <> T.pack err)]

-- | Say you liked a set. Asking twice is the same as asking once.
postApiV1ArkhamPublishedCardSetLikeR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamPublishedCardSetLikeR publishedId = do
  userId <- getRequestUserId
  row <- runDB $ get404 publishedId
  now <- liftIO getCurrentTime
  existing <- runDB $ P.getBy (UniquePublishedCardSetLike publishedId userId)
  when (isNothing existing)
    $ runDB
    $ P.insert_
    $ ArkhamPublishedCardSetLike publishedId userId now
  listingFor userId (Entity publishedId row)

-- | Take it back. Not having liked it is already the outcome, so this cannot fail.
deleteApiV1ArkhamPublishedCardSetLikeR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
deleteApiV1ArkhamPublishedCardSetLikeR publishedId = do
  userId <- getRequestUserId
  row <- runDB $ get404 publishedId
  runDB
    $ P.deleteWhere
      [ ArkhamPublishedCardSetLikePublishedCardSetId P.==. publishedId
      , ArkhamPublishedCardSetLikeUserId P.==. userId
      ]
  listingFor userId (Entity publishedId row)

{- | A card's place in its set: the number printed on it, read out of its own def.

Sorted on the leading digits rather than the text, so 10 follows 9. A card with no
number sorts last, since it has said nothing about where it goes.
-}
printedOrder :: Value -> (Int, Text)
printedOrder value = (maybe maxBound id (readMaybe (T.unpack digits)), number)
 where
  number = case value of
    Object o -> case KeyMap.lookup "meta" o of
      Just (Object m) -> case KeyMap.lookup "number" m of
        Just (String t) -> t
        Just (Number n) -> tshow (round n :: Int)
        _ -> ""
      _ -> ""
    _ -> ""
  digits = T.takeWhile isDigit number

{- | How many cards a listing carries. Enough to fill a row at any width the
listing is shown at; the extra are clipped by the layout rather than sent and
thrown away. -}
previewSize :: Int
previewSize = 12

-- | Everyone's published sets, newest first.
getApiV1ArkhamPublishedCardSetsR :: Handler [PublishedCardSetResponse]
getApiV1ArkhamPublishedCardSetsR = do
  userId <- getRequestUserId
  rows <- runDB $ P.selectList [] [P.Desc ArkhamPublishedCardSetUpdatedAt]
  traverse (listingFor userId) rows

listingFor :: UserId -> Entity ArkhamPublishedCardSet -> Handler PublishedCardSetResponse
listingFor userId (Entity publishedId row) = do
  latest <-
    runDB
      $ P.selectFirst
        [ArkhamPublishedCardSetVersionPublishedCardSetId P.==. publishedId]
        [P.Desc ArkhamPublishedCardSetVersionVersion]
  author <- runDB $ DB.get (arkhamPublishedCardSetUserId row)
  -- One of *your* sets following this listing, whichever set that is.
  mine <-
    runDB
      $ P.selectFirst [ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId] []
  subscribed <- case mine of
    Nothing -> pure Nothing
    Just (Entity _ sub) -> do
      owner <- runDB $ DB.get (arkhamCardSetSubscriptionCustomCardSetId sub)
      pure $ case owner of
        Just s | arkhamCustomCardSetUserId s == userId -> Just (arkhamCardSetSubscriptionVersion sub)
        _ -> Nothing
  cards <- case latest of
    Nothing -> pure []
    Just (Entity _ v) -> versionCards v
  likes <- runDB $ P.count [ArkhamPublishedCardSetLikePublishedCardSetId P.==. publishedId]
  liked <-
    runDB
      $ isJust
      <$> P.getBy (UniquePublishedCardSetLike publishedId userId)
  pure
    $ PublishedCardSetResponse
      { publishedCardSetResponseId = publishedId
      , publishedCardSetResponseName = arkhamPublishedCardSetName row
      , publishedCardSetResponseAuthor = maybe "someone" userUsername author
      , publishedCardSetResponseMine = arkhamPublishedCardSetUserId row == userId
      , publishedCardSetResponseLatestVersion = arkhamPublishedCardSetLatestVersion row
      , publishedCardSetResponseCardCount = length cards
      , publishedCardSetResponseNote = arkhamPublishedCardSetVersionNote . entityVal =<< latest
      , publishedCardSetResponsePreview = take previewSize cards
      , publishedCardSetResponseLikes = likes
      , publishedCardSetResponseLiked = liked
      , publishedCardSetResponseUpdatedAt = arkhamPublishedCardSetUpdatedAt row
      , publishedCardSetResponseSubscribedVersion = subscribed
      }

-- | A version to look at or import. `?version=` for an older one.
getApiV1ArkhamPublishedCardSetR :: ArkhamPublishedCardSetId -> Handler PublishedCardSetVersionResponse
getApiV1ArkhamPublishedCardSetR publishedId = do
  userId <- getRequestUserId
  published <- runDB $ get404 publishedId
  listing <- listingFor userId (Entity publishedId published)
  wanted <- lookupGetParam "version"
  row <- requireVersion publishedId (readMaybe . T.unpack =<< wanted)
  cards <- versionCards row
  pure
    $ PublishedCardSetVersionResponse
      { publishedCardSetVersionResponseListing = listing
      , publishedCardSetVersionResponseVersion = arkhamPublishedCardSetVersionVersion row
      , publishedCardSetVersionResponseName = arkhamPublishedCardSetVersionName row
      , publishedCardSetVersionResponseNote = arkhamPublishedCardSetVersionNote row
      , publishedCardSetVersionResponseCards = cards
      , publishedCardSetVersionResponseCreatedAt = arkhamPublishedCardSetVersionCreatedAt row
      }

requireVersion
  :: ArkhamPublishedCardSetId -> Maybe Int -> Handler ArkhamPublishedCardSetVersion
requireVersion publishedId wanted = do
  found <- case wanted of
    Just version ->
      runDB $ P.getBy (UniquePublishedCardSetVersion publishedId version)
    Nothing ->
      runDB
        $ P.selectFirst
          [ArkhamPublishedCardSetVersionPublishedCardSetId P.==. publishedId]
          [P.Desc ArkhamPublishedCardSetVersionVersion]
  case found of
    Just (Entity _ row) -> pure row
    Nothing -> notFound

{- | Unlist a set. The versions go with it, and anyone following it is
unsubscribed by the foreign key rather than left pointing at nothing.
-}
deleteApiV1ArkhamPublishedCardSetR :: ArkhamPublishedCardSetId -> Handler ()
deleteApiV1ArkhamPublishedCardSetR publishedId = do
  userId <- getRequestUserId
  row <- runDB $ get404 publishedId
  unless (arkhamPublishedCardSetUserId row == userId) $ permissionDenied "Not your published set"
  runDB do
    P.deleteWhere [ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId]
    P.deleteWhere [ArkhamPublishedCardSetVersionPublishedCardSetId P.==. publishedId]
    P.delete publishedId

{- | Publish the set as a new version.

The cards are snapshotted as they stand, so the version stays importable however
the author's own copy changes afterwards.
-}
postApiV1ArkhamCustomCardSetPublishR
  :: ArkhamCustomCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamCustomCardSetPublishR setId = do
  userId <- getRequestUserId
  setRow <- ownedCardSet userId setId
  PublishPost {publishNote} <- requireCheckJsonBody
  now <- liftIO getCurrentTime

  cards <- runDB $ P.selectList [ArkhamCustomCardCustomCardSetId P.==. setId] []
  when (null cards) $ invalidArgs ["There is nothing in that set to publish"]

  let name = arkhamCustomCardSetName setRow
      -- Printed order, which is the order the author put them in. It is fixed
      -- here rather than by whoever reads the version, because the listing only
      -- carries the first few and they have to be the first few.
      ordered = sortOn (printedOrder . arkhamCustomCardDef . entityVal) cards
      snapshot =
        toJSON
          [ object ["def" .= arkhamCustomCardDef card, "art" .= arkhamCustomCardArt card]
          | Entity _ card <- ordered
          ]

  existing <-
    runDB
      $ P.selectFirst
        [ ArkhamPublishedCardSetUserId P.==. userId
        , ArkhamPublishedCardSetCustomCardSetId P.==. Just setId
        ]
        []

  publishedId <- runDB case existing of
    Just (Entity found row) -> do
      let version = arkhamPublishedCardSetLatestVersion row + 1
      P.update
        found
        [ ArkhamPublishedCardSetName P.=. name
        , ArkhamPublishedCardSetLatestVersion P.=. version
        , ArkhamPublishedCardSetUpdatedAt P.=. now
        ]
      P.insert_
        $ ArkhamPublishedCardSetVersion found version publishNote name snapshot now
      pure found
    Nothing -> do
      found <- P.insert $ ArkhamPublishedCardSet userId (Just setId) name 1 now now
      P.insert_ $ ArkhamPublishedCardSetVersion found 1 publishNote name snapshot now
      pure found

  row <- runDB $ get404 publishedId
  listingFor userId (Entity publishedId row)

{- | Take a published set into your own collection, following it from then on.

The import is the file import's: prepare every card (which is where art is
uploaded and can fail) before emptying and refilling the set, so a set that is
already here is never left as half of what was replacing it.
-}
postApiV1ArkhamPublishedCardSetSubscribeR
  :: ArkhamPublishedCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamPublishedCardSetSubscribeR publishedId = do
  userId <- getRequestUserId
  published <- runDB $ get404 publishedId
  SubscribePost {subscribeVersion} <- requireCheckJsonBody
  version <- requireVersion publishedId subscribeVersion
  void $ importVersion userId publishedId version
  listingFor userId (Entity publishedId published)

{- | Bring a subscribed set up to the newest published version.

Only a set that is still following one can be updated; an edited copy has to be
taken again from the marketplace instead, which is what its lost subscription
means.
-}
postApiV1ArkhamCustomCardSetSyncR
  :: ArkhamCustomCardSetId -> Handler PublishedCardSetResponse
postApiV1ArkhamCustomCardSetSyncR setId = do
  userId <- getRequestUserId
  void $ ownedCardSet userId setId
  subscription <- runDB $ P.getBy (UniqueCardSetSubscription setId)
  case subscription of
    Nothing -> invalidArgs ["That set is not following a published one any more"]
    Just (Entity _ sub) -> do
      let publishedId = arkhamCardSetSubscriptionPublishedCardSetId sub
      published <- runDB $ get404 publishedId
      version <- requireVersion publishedId Nothing
      void $ importVersion userId publishedId version
      listingFor userId (Entity publishedId published)

{- | Write a published version into the caller's collection and record that it is
following it. The set is matched by the subscription that already points at it,
so updating replaces the copy in place rather than making a second one.
-}
importVersion
  :: UserId
  -> ArkhamPublishedCardSetId
  -> ArkhamPublishedCardSetVersion
  -> Handler ArkhamCustomCardSetId
importVersion userId publishedId version = do
  cards <- versionCards version
  now <- liftIO getCurrentTime
  let name = arkhamPublishedCardSetVersionName version

  existing <- mineFollowing userId publishedId
  -- A first subscription has to find a name nobody else of yours is using.
  name' <- case existing of
    Just _ -> pure name
    Nothing -> freeName userId name

  prepared <- traverse (prepareCardForSet userId name') cards

  runDB do
    setId <- case existing of
      Just found -> do
        P.update
          found
          [ArkhamCustomCardSetName P.=. name', ArkhamCustomCardSetUpdatedAt P.=. now]
        P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. found]
        pure found
      Nothing -> P.insert $ ArkhamCustomCardSet userId name' Nothing now now
    for_ prepared $ persistCard userId setId now
    -- Written after the cards, so a failed import leaves nothing claiming to be
    -- up to date.
    P.deleteWhere [ArkhamCardSetSubscriptionCustomCardSetId P.==. setId]
    P.insert_
      $ ArkhamCardSetSubscription
        setId
        publishedId
        (arkhamPublishedCardSetVersionVersion version)
        now
        now
    pure setId

-- | The caller's own set following this listing, if they have one.
mineFollowing :: UserId -> ArkhamPublishedCardSetId -> Handler (Maybe ArkhamCustomCardSetId)
mineFollowing userId publishedId = do
  subs <-
    runDB $ P.selectList [ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId] []
  ours <- forM subs \(Entity _ sub) -> do
    let setId = arkhamCardSetSubscriptionCustomCardSetId sub
    owner <- runDB $ DB.get setId
    pure $ case owner of
      Just s | arkhamCustomCardSetUserId s == userId -> Just setId
      _ -> Nothing
  pure $ listToMaybe (catMaybes ours)

{- | A name no other set of yours holds. Subscribing to something called what one
of your own sets is called should not fail, and should not silently replace it.
-}
freeName :: UserId -> Text -> Handler Text
freeName userId name = go name (2 :: Int)
 where
  go candidate attempt = do
    taken <- runDB $ P.getBy (UniqueUserCustomCardSetName userId candidate)
    case taken of
      Nothing -> pure candidate
      Just _
        | attempt > 50 -> invalidArgs ["You already have a set called \"" <> name <> "\""]
        | otherwise -> go (name <> " (" <> tshow attempt <> ")") (attempt + 1)
