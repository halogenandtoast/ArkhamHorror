{- | Every custom card set on the instance, as a browsable list.

'Api.Handler.Arkham.CardSetSubmissions' next door is the review queue: only the
sets somebody chose to publish, and only the versions they submitted. This is
the other question -- what is actually being built here -- which that queue
cannot answer, because most sets are never submitted at all.

Paginated rather than listed, since this grows with every account, and ordered
by what was last edited: a browser opened to see what people are making wants
the set somebody touched this morning, not the oldest row in the table.

Most sets are copies. Subscribing to a marketplace listing, or taking a set
through a share link, leaves a row here per subscriber, so an unfiltered list is
mostly the same handful of sets over and over. @scope@ is what makes it
readable, and @originals@ -- a set that follows nothing -- is the default.

A row carries a few cards so the list can be skimmed, and the rest are behind
the detail endpoint, the same split the review queue makes for the same reason.

Both of these are under @\/api\/v1\/admin@, which 'Foundation.isAuthorized' gates
on the admin flag, so neither checks it again.
-}
module Api.Handler.Arkham.Admin.CustomCardSets (
  getApiV1AdminCustomCardSetsR,
  getApiV1AdminCustomCardSetR,
) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Database.Esqueleto.Experimental
import Database.Persist qualified as P

-- \| 'Value' is esqueleto's here; aeson's is reached through the qualified
-- import. 'exists' and 'isNothing' likewise: persistent's 'exists' takes a filter
-- list, and every use of both here is over a column in a subquery.
import Import hiding (Value, exists, isNothing, on, (==.), (||.))
import Json hiding (Value)

{- | How many sets a page holds.

Twenty rather than the fifty a table of names could take, because a row carries
pictures: twenty rows of 'previewSize' is already a hundred and twenty card
images on one page.
-}
perPage :: Int64
perPage = 20

{- | As many cards as a row can usefully show.

Deliberately smaller than the review queue's twelve. A reviewer is looking at
one set and needs to see what is in it; this list is answering "what is this",
and a handful of cards does that at a fifth of the page weight.
-}
previewSize :: Int
previewSize = 6

{- | A card as the strip draws it: @{def, art}@, the shape the builder's own
library and the marketplace both use.

The def is passed through as the stored JSON rather than decoded into a 'CardDef'
and re-encoded. Decoding can fail -- a def written by an older builder, or one
saved against a DSL this binary no longer has -- and on this page that failure
would be a 500 over somebody else's broken card, taking the whole list with it.
A browser should still show the set, and the card should still draw whatever its
art is.
-}
data SetCard = SetCard
  { setCardDef :: Aeson.Value
  , setCardArt :: Maybe Text
  }

instance ToJSON SetCard where
  toJSON SetCard {..} = object ["def" .= setCardDef, "art" .= setCardArt]

{- | What this set follows, for one that is a copy of somebody else's.

@marketplace@ is a subscription to a listing, @link@ a set taken through a share
link. Both are locked copies in their owner's builder, and neither is anything
that account made.
-}
data Follows = FollowsMarketplace | FollowsLink

instance ToJSON Follows where
  toJSON FollowsMarketplace = "marketplace"
  toJSON FollowsLink = "link"

data CardSetRow = CardSetRow
  { cardSetRowId :: ArkhamCustomCardSetId
  , cardSetRowName :: Text
  , cardSetRowDescription :: Maybe Text
  , cardSetRowUrl :: Maybe Text
  -- ^ Where the author says the set lives, which is the one link on the row.
  , cardSetRowIcon :: Maybe Text
  , cardSetRowInvestigatorExpansion :: Bool
  , cardSetRowSourceCode :: Maybe Text
  -- ^ Set when the set was imported from an arkham.build pack rather than built here.
  , cardSetRowUserId :: UserId
  , cardSetRowAuthor :: Text
  , cardSetRowAuthorEmail :: Text
  {- ^ Shown because it is searched, the same reason the users panel shows it:
  a row that matched on email has to say so.
  -}
  , cardSetRowCardCount :: Int
  , cardSetRowCreatedAt :: UTCTime
  , cardSetRowUpdatedAt :: UTCTime
  , cardSetRowFollows :: Maybe Follows
  -- ^ Absent on a set of the account's own, which is what 'FollowsMarketplace' is not.
  , cardSetRowPublishedCardSetId :: Maybe ArkhamPublishedCardSetId
  {- ^ This set's own marketplace listing, if its author has published it, and
  where that listing stands. Distinct from 'cardSetRowFollows', which is a
  listing of somebody else's.
  -}
  , cardSetRowApprovedVersion :: Maybe Int
  , cardSetRowSubmissionVersion :: Maybe Int
  , cardSetRowSubmissionStatus :: Maybe Text
  , cardSetRowPreview :: [SetCard]
  }
  deriving stock Generic

instance ToJSON CardSetRow where
  toJSON = genericToJSON $ aesonOptions $ Just "cardSetRow"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cardSetRow"

{- | A page of sets, and how many there are to page through.

@total@ is the count under the same filters as the rows, so the page count the
client works out from it is the page count it can actually reach.
-}
data CardSetsResponse = CardSetsResponse
  { cardSetsResponseSets :: [CardSetRow]
  , cardSetsResponseTotal :: Int
  , cardSetsResponsePage :: Int
  , cardSetsResponsePerPage :: Int
  }
  deriving stock Generic

instance ToJSON CardSetsResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "cardSetsResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cardSetsResponse"

-- | One set with every card in it, which is what actually looking at one takes.
data CardSetDetailResponse = CardSetDetailResponse
  { cardSetDetailResponseSet :: CardSetRow
  , cardSetDetailResponseCards :: [SetCard]
  }
  deriving stock Generic

instance ToJSON CardSetDetailResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "cardSetDetailResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cardSetDetailResponse"

{- | Which sets the list is asking for.

'ScopeOriginals' is the default because it is the only one of these that answers
"what is being built here": the other two are dominated by copies of a few
popular sets.
-}
data Scope = ScopeAll | ScopeOriginals | ScopeCopies | ScopePublished

parseScope :: Maybe Text -> Scope
parseScope = \case
  Just "all" -> ScopeAll
  Just "copies" -> ScopeCopies
  Just "published" -> ScopePublished
  _ -> ScopeOriginals

{- | The set follows a marketplace listing.

Only a subscription that is still followed: the row outlives that -- it is also
"this set of mine is my copy of that listing", which is how taking it again
replaces the copy -- but a copy taken into the editor is an ordinary set of its
owner's and follows nothing.
-}
followsListing :: SqlExpr (Entity ArkhamCustomCardSet) -> SqlExpr (Value Bool)
followsListing sets = exists do
  sub <- from $ table @ArkhamCardSetSubscription
  where_ $ sub.customCardSetId ==. sets.id &&. sub.subscribed ==. val True

{- | The set was taken through somebody's share link, and still follows it.

A revoked or unsubscribed access row is not a copy any more: there is nothing at
the other end of the link.
-}
followsShare :: SqlExpr (Entity ArkhamCustomCardSet) -> SqlExpr (Value Bool)
followsShare sets = exists do
  access <- from $ table @ArkhamCardSetShareAccess
  where_
    $ access.customCardSetId
    ==. just sets.id
    &&. access.subscribed ==. val True
    &&. isNothing access.revokedAt

-- | The set has a marketplace listing of its own, approved or not.
hasListing :: SqlExpr (Entity ArkhamCustomCardSet) -> SqlExpr (Value Bool)
hasListing sets = exists do
  listing <- from $ table @ArkhamPublishedCardSet
  where_ $ listing.customCardSetId ==. just sets.id

getApiV1AdminCustomCardSetsR :: Handler CardSetsResponse
getApiV1AdminCustomCardSetsR = do
  query <- maybe "" T.strip <$> lookupGetParam "q"
  scope <- parseScope <$> lookupGetParam "scope"
  -- A page below the first is the first; one past the end is an empty page,
  -- which is what the client's own arithmetic would have asked for anyway.
  page <- maybe 1 (max 1) . (>>= readMaybe . T.unpack) <$> lookupGetParam "page"

  let
    -- Lowered on both sides rather than matched with ILIKE, and anywhere in the
    -- value: an admin is typing a set or a person half-remembered, not
    -- reproducing the case and the prefix it was stored under.
    matches sets users =
      let fragment = (%) ++. val (T.toLower query) ++. (%)
       in (lower_ sets.name `like` fragment)
            ||. (lower_ users.username `like` fragment)
            ||. (lower_ users.email `like` fragment)

    inScope sets = case scope of
      ScopeAll -> val True
      ScopeOriginals -> not_ (followsListing sets) &&. not_ (followsShare sets)
      ScopeCopies -> followsListing sets ||. followsShare sets
      ScopePublished -> hasListing sets

    filtered = do
      (sets :& users) <-
        from
          $ table @ArkhamCustomCardSet
          `innerJoin` table @User
            `on` (\(sets :& users) -> sets.userId ==. users.id)
      where_ $ inScope sets
      unless (T.null query) $ where_ (matches sets users)
      pure (sets, users)

  (total, rows) <- runDB do
    total <-
      fmap (maybe 0 unValue) . selectOne $ do
        _ <- filtered
        pure (countRows @Int)

    rows <- select do
      (sets, users) <- filtered
      orderBy [desc sets.updatedAt]
      offset $ (fromIntegral page - 1) * perPage
      limit perPage
      -- The two booleans come back rather than being asked again per row: they
      -- are the same predicates the scope filter just ran, and a row has to say
      -- whether it is a copy on every scope, not only on `copies`.
      pure (sets, users, followsListing sets, followsShare sets)

    pure (total, rows)

  let setIds = [entityKey sets | (sets, _, _, _) <- rows]

  -- One transaction for everything the page needs, rather than one per lookup:
  -- the preview is a query per row, and twenty of those should not be twenty
  -- round trips to the pool.
  (counts, listings, submissions, previewMap) <- runDB do
    counts <- cardCounts setIds
    listings <- listingsFor setIds
    submissions <- latestSubmissions (map entityKey (Map.elems listings))
    previews <- traverse (\setId -> fmap (\cards -> (setId, cards)) (previewCards setId)) setIds
    pure (counts, listings, submissions, Map.fromList previews)

  let toRow (Entity setId row, Entity userId user, Value listed, Value shared) =
        let listing = Map.lookup setId listings
            submission = listing >>= \(Entity listingId _) -> Map.lookup listingId submissions
         in CardSetRow
              { cardSetRowId = setId
              , cardSetRowName = row.name
              , cardSetRowDescription = row.description
              , cardSetRowUrl = row.url
              , cardSetRowIcon = row.icon
              , cardSetRowInvestigatorExpansion = row.investigatorExpansion
              , cardSetRowSourceCode = row.sourceCode
              , cardSetRowUserId = userId
              , cardSetRowAuthor = user.username
              , cardSetRowAuthorEmail = user.email
              , cardSetRowCardCount = fromMaybe 0 $ Map.lookup setId counts
              , cardSetRowCreatedAt = row.createdAt
              , cardSetRowUpdatedAt = row.updatedAt
              , cardSetRowFollows =
                  if listed
                    then Just FollowsMarketplace
                    else if shared then Just FollowsLink else Nothing
              , cardSetRowPublishedCardSetId = entityKey <$> listing
              , cardSetRowApprovedVersion = (\(Entity _ l) -> l.approvedVersion) =<< listing
              , cardSetRowSubmissionVersion = arkhamCardSetSubmissionVersion <$> submission
              , cardSetRowSubmissionStatus = arkhamCardSetSubmissionStatus <$> submission
              , cardSetRowPreview = fromMaybe [] $ Map.lookup setId previewMap
              }

  pure
    $ CardSetsResponse
      { cardSetsResponseSets = map toRow rows
      , cardSetsResponseTotal = total
      , cardSetsResponsePage = page
      , cardSetsResponsePerPage = fromIntegral perPage
      }

-- | One set, with all of its cards.
getApiV1AdminCustomCardSetR :: ArkhamCustomCardSetId -> Handler CardSetDetailResponse
getApiV1AdminCustomCardSetR setId = do
  row <- runDB $ get404 setId
  user <- runDB $ get404 row.userId
  listed <-
    runDB
      $ P.exists
        [ ArkhamCardSetSubscriptionCustomCardSetId P.==. setId
        , ArkhamCardSetSubscriptionSubscribed P.==. True
        ]
  shared <-
    runDB
      $ P.exists
        [ ArkhamCardSetShareAccessCustomCardSetId P.==. Just setId
        , ArkhamCardSetShareAccessSubscribed P.==. True
        , ArkhamCardSetShareAccessRevokedAt P.==. Nothing
        ]
  counts <- runDB $ cardCounts [setId]
  listings <- runDB $ listingsFor [setId]
  submissions <- runDB $ latestSubmissions (map entityKey (Map.elems listings))
  cards <- runDB $ allCards setId

  let listing = Map.lookup setId listings
      submission = listing >>= \(Entity listingId _) -> Map.lookup listingId submissions
  pure
    $ CardSetDetailResponse
      { cardSetDetailResponseSet =
          CardSetRow
            { cardSetRowId = setId
            , cardSetRowName = row.name
            , cardSetRowDescription = row.description
            , cardSetRowUrl = row.url
            , cardSetRowIcon = row.icon
            , cardSetRowInvestigatorExpansion = row.investigatorExpansion
            , cardSetRowSourceCode = row.sourceCode
            , cardSetRowUserId = row.userId
            , cardSetRowAuthor = user.username
            , cardSetRowAuthorEmail = user.email
            , cardSetRowCardCount = fromMaybe (length cards) $ Map.lookup setId counts
            , cardSetRowCreatedAt = row.createdAt
            , cardSetRowUpdatedAt = row.updatedAt
            , cardSetRowFollows =
                if listed
                  then Just FollowsMarketplace
                  else if shared then Just FollowsLink else Nothing
            , cardSetRowPublishedCardSetId = entityKey <$> listing
            , cardSetRowApprovedVersion = (\(Entity _ l) -> l.approvedVersion) =<< listing
            , cardSetRowSubmissionVersion = arkhamCardSetSubmissionVersion <$> submission
            , cardSetRowSubmissionStatus = arkhamCardSetSubmissionStatus <$> submission
            , cardSetRowPreview = take previewSize cards
            }
      , cardSetDetailResponseCards = cards
      }

{- | How many cards each of these sets holds, in one grouped query rather than a
count per row.
-}
cardCounts
  :: MonadIO m => [ArkhamCustomCardSetId] -> SqlPersistT m (Map ArkhamCustomCardSetId Int)
cardCounts [] = pure mempty
cardCounts setIds = do
  rows <- select do
    cards <- from $ table @ArkhamCustomCard
    where_ $ cards.customCardSetId `in_` valList setIds
    groupBy cards.customCardSetId
    pure (cards.customCardSetId, countRows @Int)
  pure $ Map.fromList [(setId, n) | (Value setId, Value n) <- rows]

{- | The marketplace listing each of these sets has of its own, keyed by the set.

Found by the listing pointing back at the set, which is how publishing again
finds the listing to add a version to.
-}
listingsFor
  :: MonadIO m
  => [ArkhamCustomCardSetId]
  -> SqlPersistT m (Map ArkhamCustomCardSetId (Entity ArkhamPublishedCardSet))
listingsFor [] = pure mempty
listingsFor setIds = do
  rows <-
    P.selectList
      [ArkhamPublishedCardSetCustomCardSetId P.<-. map Just setIds]
      []
  pure $ Map.fromList [(setId, row) | row@(Entity _ l) <- rows, Just setId <- [l.customCardSetId]]

{- | The newest submission against each of these listings.

Highest version wins, which is the same row 'Api.Handler.Arkham.CustomCardSets'
picks when it reports where a set stands: ordered ascending here so that the
later insert into the map is the one kept.
-}
latestSubmissions
  :: MonadIO m
  => [ArkhamPublishedCardSetId]
  -> SqlPersistT m (Map ArkhamPublishedCardSetId ArkhamCardSetSubmission)
latestSubmissions [] = pure mempty
latestSubmissions listingIds = do
  rows <-
    P.selectList
      [ArkhamCardSetSubmissionPublishedCardSetId P.<-. listingIds]
      [P.Asc ArkhamCardSetSubmissionVersion]
  pure $ Map.fromList [(s.publishedCardSetId, s) | Entity _ s <- rows]

{- | A few of a set's cards, for the row.

Printed order is the def's @meta.number@, which is not a column, so this takes
the oldest few and leaves the ordering to the client -- which already sorts the
review queue's previews the same way. A preview is a sample either way.
-}
previewCards :: MonadIO m => ArkhamCustomCardSetId -> SqlPersistT m [SetCard]
previewCards setId = do
  rows <-
    P.selectList
      [ArkhamCustomCardCustomCardSetId P.==. setId]
      [P.Asc ArkhamCustomCardCreatedAt, P.LimitTo previewSize]
  pure $ map toSetCard rows

-- | Every card in a set, which is what the detail endpoint is for.
allCards :: MonadIO m => ArkhamCustomCardSetId -> SqlPersistT m [SetCard]
allCards setId = do
  rows <-
    P.selectList
      [ArkhamCustomCardCustomCardSetId P.==. setId]
      [P.Asc ArkhamCustomCardCreatedAt]
  pure $ map toSetCard rows

toSetCard :: Entity ArkhamCustomCard -> SetCard
toSetCard (Entity _ card) = SetCard {setCardDef = card.def, setCardArt = card.art}
