{- | Handing an unpublished set to a few people.

The marketplace is the other way to put a set in somebody else's hands, and it is
the wrong one for a set that is not finished: it is public, every version is
reviewed before anyone can take it, and what playtesting wants is for the people
trying the cards to have whatever was changed this afternoon. So nothing here is
versioned -- a share hands out the author's set as it stands, and taking it again
is how a friend catches up.

A share is a capability url: one per set, which the author sends to whoever they
like. Who holds it is therefore not something the author declares, and the only
record of it is who has opened the link -- which is what
'ArkhamCardSetShareAccess' is, and what makes "who has access" answerable at all.

Taking a copy runs the same import the marketplace and a file import run, so the
cards land in the recipient's own library under their own codes and can be
deckbuilt with and played like any others.

There is only one way to take it, and it is subscribing: the copy is locked, so
it stays what the author shared, and subscribing again is how it catches up with
them. The marketplace's other way -- importing a copy of your own, to edit and
keep -- is not offered here and neither is "Add to my editor", because this set
is unpublished. It was handed to a few people to try, not given away: a copy
somebody could edit, export and pass on is the author's unreviewed work loose in
the world, which is the one thing a private link is for not doing.

What a recipient can do with it is play: the cards are in their library under
their own codes, deckbuildable and playable like any others. What they cannot do
is make it theirs. Deleting it is the way to be rid of it.
-}
module Api.Handler.Arkham.CardSetShares (
  getApiV1ArkhamCustomCardSetShareR,
  postApiV1ArkhamCustomCardSetShareR,
  deleteApiV1ArkhamCustomCardSetShareR,
  putApiV1ArkhamCustomCardSetShareAccessR,
  getApiV1ArkhamSharedCardSetR,
  postApiV1ArkhamSharedCardSetSubscribeR,
  shareFor,
) where

import Api.Handler.Arkham.CustomCards (
  Source (..),
  followOnly,
  freeName,
  myCopyOfSet,
  persistCard,
  prepareCardForSet,
  printedOrder,
  requireUnlocked,
  setDeclarationsFromCards,
 )
import Arkham.Card.CustomCard (CustomCard (..))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as BSL
import Data.Time.Clock
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.Persist qualified as DB
import Import hiding ((==.))
import Import qualified as P
import Json hiding (Success)

-- | The link, and everyone it has reached. Only ever the author's own view.
data CardSetShareResponse = CardSetShareResponse
  { cardSetShareResponseSetId :: ArkhamCustomCardSetId
  , cardSetShareResponseToken :: Text
  {- ^ The secret in the url. Sent back in full on every read, because the
  author has to be able to copy the link again to send it to a second friend --
  which is also why it is stored rather than hashed.
  -}
  , cardSetShareResponseCreatedAt :: UTCTime
  , cardSetShareResponseUpdatedAt :: UTCTime
  -- ^ When this link was last minted, so "a new link" can be seen to have worked.
  , cardSetShareResponsePeople :: [CardSetShareAccessResponse]
  }
  deriving stock Generic

instance ToJSON CardSetShareResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "cardSetShareResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cardSetShareResponse"

{- | One person the link reached, and where they stand with the set.

Everything but the name is read off the two set rows rather than stored: there is
no "they are up to date" flag for an edit to the author's set to go stale
against.
-}
data CardSetShareAccessResponse = CardSetShareAccessResponse
  { cardSetShareAccessResponseId :: ArkhamCardSetShareAccessId
  , cardSetShareAccessResponseName :: Text
  , cardSetShareAccessResponseTakenAt :: Maybe UTCTime
  -- ^ When they last took a copy. Null for somebody who has only looked.
  , cardSetShareAccessResponseBehind :: Bool
  {- ^ Whether the author has changed the set since this person took it. False
  for somebody who has not taken it at all -- they are not behind, they have
  nothing.
  -}
  , cardSetShareAccessResponseRevoked :: Bool
  , cardSetShareAccessResponseFirstSeenAt :: UTCTime
  , cardSetShareAccessResponseLastSeenAt :: UTCTime
  }
  deriving stock Generic

instance ToJSON CardSetShareAccessResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "cardSetShareAccessResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "cardSetShareAccessResponse"

{- | A shared set as the person holding the link sees it: what it is, every card
in it, and what they have of it.

The whole set, not a preview: the point of the link is to decide whether to try
the cards, and the cards are the set.
-}
data SharedCardSetResponse = SharedCardSetResponse
  { sharedCardSetResponseName :: Text
  , sharedCardSetResponseDescription :: Maybe Text
  , sharedCardSetResponseUrl :: Maybe Text
  , sharedCardSetResponseIcon :: Maybe Text
  , sharedCardSetResponseInvestigatorExpansion :: Bool
  , sharedCardSetResponseAuthor :: Text
  , sharedCardSetResponseMine :: Bool
  -- ^ The author following their own link, who has nothing to take.
  , sharedCardSetResponseCards :: [CustomCard]
  , sharedCardSetResponseUpdatedAt :: UTCTime
  -- ^ When the author last changed the set, which is what "new" is measured from.
  , sharedCardSetResponseSetId :: Maybe ArkhamCustomCardSetId
  {- ^ Your own copy, if you have taken one: the set in your library the
  builder should open.
  -}
  , sharedCardSetResponseListing :: Maybe ArkhamPublishedCardSetId
  {- ^ The marketplace listing for this set, once the author has published it
  and a version has been approved.

  Worth sending because it changes what the holder of the link may do. A shared
  set is unpublished, so a copy of it stays the author's; a published one is
  public, so subscribing to the /listing/ gives an ordinary marketplace copy --
  one that can be taken into the editor and exported. Subscribing from the
  marketplace adopts the copy they already have rather than making a second,
  but they have to know to go and do it.
  -}
  , sharedCardSetResponseSubscribed :: Bool
  {- ^ Whether that copy follows this set, which it does if it exists: a shared
  set can only be subscribed to. Sent so the page can say so rather than infer
  it, and so a copy taken before that was the only way reads honestly.
  -}
  , sharedCardSetResponseBehind :: Bool
  {- ^ The author has changed the set since you took it, which is the whole of
  whether there is anything to do here: subscribing again is how it catches up,
  and when it is level there is nothing to take and no button for it.
  -}
  }
  deriving stock Generic

instance ToJSON SharedCardSetResponse where
  toJSON = genericToJSON $ aesonOptions $ Just "sharedCardSetResponse"
  toEncoding = genericToEncoding $ aesonOptions $ Just "sharedCardSetResponse"

-- | What a POST to the link says: mint a new one, or leave the one that is there.
newtype SharePost = SharePost {shareRotate :: Maybe Bool}
  deriving stock Generic

instance FromJSON SharePost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "share"

-- | Cutting one person off, or letting them back in.
newtype AccessPut = AccessPut {accessRevoked :: Bool}
  deriving stock Generic

instance FromJSON AccessPut where
  parseJSON = genericParseJSON $ aesonOptions $ Just "access"

{- | The secret in a share url: 16 random bytes, base64url.

A version-4 UUID's raw bytes, drawn from the system CSPRNG, for the same reason
"Auth.ApiKey" uses them -- already a dependency, and no new class of @MonadRandom@
to disambiguate. 122 bits is far past guessing a link out of the air, and keeps
the url short enough to paste into a chat window without it wrapping.
-}
mintToken :: MonadIO m => m Text
mintToken = do
  uuid <- liftIO UUID.nextRandom
  pure . decodeUtf8 . B64URL.encodeUnpadded . BSL.toStrict $ UUID.toByteString uuid

{- | The set at this id, if it is the caller's, along with its share.

Separate from 'Api.Handler.Arkham.CustomCards.ownedCardSet' only in that it
answers with the share too, which every one of the author's four actions needs.
-}
ownedShare
  :: UserId
  -> ArkhamCustomCardSetId
  -> Handler (ArkhamCustomCardSet, Maybe (Entity ArkhamCardSetShare))
ownedShare userId setId = do
  set <- runDB $ get404 setId
  unless (arkhamCustomCardSetUserId set == userId) $ permissionDenied "Not your card set"
  share <- runDB $ P.getBy (UniqueCardSetShare setId)
  pure (set, share)

{- | The link and its people, as the author's page reads.

The @behind@ on each person is the author's set being newer than the copy they
took, which is the same comparison the recipient's own page makes from the other
side.
-}
shareFor :: ArkhamCustomCardSet -> Entity ArkhamCardSetShare -> Handler CardSetShareResponse
shareFor set (Entity shareId share) = do
  accesses <-
    runDB
      $ P.selectList
        [ArkhamCardSetShareAccessCardSetShareId P.==. shareId]
        [P.Asc ArkhamCardSetShareAccessCreatedAt]
  people <- for accesses \(Entity accessId access) -> do
    user <- runDB $ DB.get (arkhamCardSetShareAccessUserId access)
    pure
      $ CardSetShareAccessResponse
        { cardSetShareAccessResponseId = accessId
        , cardSetShareAccessResponseName = maybe "someone" userUsername user
        , cardSetShareAccessResponseTakenAt = arkhamCardSetShareAccessTakenAt access
        , cardSetShareAccessResponseBehind =
            maybe False (< arkhamCustomCardSetUpdatedAt set) (arkhamCardSetShareAccessTakenAt access)
        , cardSetShareAccessResponseRevoked = isJust (arkhamCardSetShareAccessRevokedAt access)
        , cardSetShareAccessResponseFirstSeenAt = arkhamCardSetShareAccessCreatedAt access
        , cardSetShareAccessResponseLastSeenAt = arkhamCardSetShareAccessUpdatedAt access
        }
  pure
    $ CardSetShareResponse
      { cardSetShareResponseSetId = arkhamCardSetShareCustomCardSetId share
      , cardSetShareResponseToken = arkhamCardSetShareToken share
      , cardSetShareResponseCreatedAt = arkhamCardSetShareCreatedAt share
      , cardSetShareResponseUpdatedAt = arkhamCardSetShareUpdatedAt share
      , cardSetShareResponsePeople = people
      }

{- | The link for this set, if it is being shared.

A 404 for a set that is not: "there is no link" is the answer, and the client
asks only when the author opens the panel.
-}
getApiV1ArkhamCustomCardSetShareR :: ArkhamCustomCardSetId -> Handler CardSetShareResponse
getApiV1ArkhamCustomCardSetShareR setId = do
  userId <- getRequestUserId
  (set, share) <- ownedShare userId setId
  case share of
    Nothing -> notFound
    Just found -> shareFor set found

{- | Start sharing, or mint a new link for a set already shared.

Asking twice without @rotate@ hands back the link that is there rather than
making a second one: the author is trying to end up with a link, and there is
one. With @rotate@ the token is replaced, which is how a link that has been
passed somewhere it should not have been is made worthless.

Rotating keeps the people: they are the record of who took the set, and that is
still true of them. Somebody who should no longer have it is removed by itself
('putApiV1ArkhamCustomCardSetShareAccessR'), and the copies anyone has taken stay
theirs either way.
-}
postApiV1ArkhamCustomCardSetShareR :: ArkhamCustomCardSetId -> Handler CardSetShareResponse
postApiV1ArkhamCustomCardSetShareR setId = do
  userId <- getRequestUserId
  (set, existing) <- ownedShare userId setId
  -- Somebody else's set, taken from a listing or a link. Handing it on is theirs
  -- to do, not yours.
  requireUnlocked setId
  SharePost {shareRotate} <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  token <- mintToken
  case existing of
    Just (Entity shareId share)
      | shareRotate /= Just True -> shareFor set (Entity shareId share)
      | otherwise -> do
          runDB
            $ P.update
              shareId
              [ ArkhamCardSetShareToken P.=. token
              , ArkhamCardSetShareUpdatedAt P.=. now
              ]
          shareFor set
            . Entity shareId
            $ share {arkhamCardSetShareToken = token, arkhamCardSetShareUpdatedAt = now}
    Nothing -> do
      let row = ArkhamCardSetShare setId token now now
      shareId <- runDB $ P.insert row
      shareFor set (Entity shareId row)

{- | Stop sharing.

The link stops working and the list of who had it goes with it -- there is
nothing left to have access to. What anybody already took stays in their library:
those are their cards now, under their own codes, possibly in a deck or a game,
and taking them away is not what un-sharing means.
-}
deleteApiV1ArkhamCustomCardSetShareR :: ArkhamCustomCardSetId -> Handler ()
deleteApiV1ArkhamCustomCardSetShareR setId = do
  userId <- getRequestUserId
  (_, share) <- ownedShare userId setId
  for_ share \(Entity shareId _) -> runDB do
    P.deleteWhere [ArkhamCardSetShareAccessCardSetShareId P.==. shareId]
    P.delete shareId

{- | Cut one person off, or let them back in.

Their row stays either way: it is the record that they have the set, and
deleting it would only mean the link let them straight back in with nothing to
show it had ever happened.
-}
putApiV1ArkhamCustomCardSetShareAccessR
  :: ArkhamCustomCardSetId -> ArkhamCardSetShareAccessId -> Handler CardSetShareResponse
putApiV1ArkhamCustomCardSetShareAccessR setId accessId = do
  userId <- getRequestUserId
  (set, share) <- ownedShare userId setId
  found@(Entity shareId _) <- maybe notFound pure share
  AccessPut {accessRevoked} <- requireCheckJsonBody
  access <- runDB $ get404 accessId
  -- Reached through the set it belongs to, so one author cannot touch another's
  -- row by guessing an id.
  unless (arkhamCardSetShareAccessCardSetShareId access == shareId) notFound
  now <- liftIO getCurrentTime
  runDB
    $ P.update
      accessId
      [ ArkhamCardSetShareAccessRevokedAt P.=. (if accessRevoked then Just now else Nothing)
      , ArkhamCardSetShareAccessUpdatedAt P.=. now
      ]
  shareFor set found

{- | The set behind a link, and the access row recording that you have followed it.

Opening the link is what grants access: there is no invitation addressed to
anyone, so the first read is the grant, and the author's list is built from these
rows. A read by the author of their own set records nothing -- they do not have
access to their own set, they have the set.

A revoked row is a 404 rather than a 403: the author has decided this person
cannot have the set, and "there is no such link" is both true from here and the
one answer that does not invite an argument.
-}
sharedSet
  :: UserId
  -> Text
  -> Handler (ArkhamCustomCardSetId, ArkhamCustomCardSet, Maybe (Entity ArkhamCardSetShareAccess))
sharedSet userId token = do
  Entity shareId share <-
    runDB (P.getBy (UniqueCardSetShareToken token)) >>= maybe notFound pure
  let setId = arkhamCardSetShareCustomCardSetId share
  set <- runDB $ get404 setId
  if arkhamCustomCardSetUserId set == userId
    then pure (setId, set, Nothing)
    else do
      now <- liftIO getCurrentTime
      existing <- runDB $ P.getBy (UniqueCardSetShareAccess shareId userId)
      case existing of
        Just (Entity _ access) | isJust (arkhamCardSetShareAccessRevokedAt access) -> notFound
        Just (Entity accessId access) -> do
          runDB $ P.update accessId [ArkhamCardSetShareAccessUpdatedAt P.=. now]
          pure
            ( setId
            , set
            , Just . Entity accessId $ access {arkhamCardSetShareAccessUpdatedAt = now}
            )
        Nothing -> do
          -- Nothing taken yet, so nothing followed: this row is somebody who has
          -- opened the link and not decided what to do with it.
          let row = ArkhamCardSetShareAccess shareId userId Nothing False Nothing Nothing now now
          accessId <- runDB $ P.insert row
          pure (setId, set, Just (Entity accessId row))

-- | A set's cards as a recipient reads them, in the order the author put them in.
setCards :: ArkhamCustomCardSetId -> Handler [CustomCard]
setCards setId = do
  rows <- runDB $ P.selectList [ArkhamCustomCardCustomCardSetId P.==. setId] []
  pure
    [ CustomCard def (arkhamCustomCardArt row)
    | Entity _ row <- sortOn (printedOrder . arkhamCustomCardDef . entityVal) rows
    , Just def <- [parseMaybe parseJSON (arkhamCustomCardDef row)]
    ]

getApiV1ArkhamSharedCardSetR :: Text -> Handler SharedCardSetResponse
getApiV1ArkhamSharedCardSetR token = do
  userId <- getRequestUserId
  (setId, set, access) <- sharedSet userId token
  cards <- setCards setId
  sharedResponse userId setId set cards access

{- | Follow the author's set: take it, and keep taking it.

The only way to take a shared set, and it leaves the copy locked, so it stays
what they shared. Subscribing again is how it catches up with them later.
-}
postApiV1ArkhamSharedCardSetSubscribeR :: Text -> Handler SharedCardSetResponse
postApiV1ArkhamSharedCardSetSubscribeR = takeShared

{- | Write the author's set into the caller's collection.

The same import the marketplace and a file import run: every card is prepared
first -- which is where art is uploaded and can fail -- and the copy is then
emptied and refilled in one transaction, so a failure leaves whatever was there
whole rather than half-replaced.

Taking it again replaces the copy in place, which is what makes this the way to
catch up with the author. Nothing of the recipient's is lost by that: the copy
is locked, so there is nothing in it they put there.
-}
takeShared :: Text -> Handler SharedCardSetResponse
takeShared token = do
  userId <- getRequestUserId
  (setId, set, access) <- sharedSet userId token
  Entity accessId accessRow <- case access of
    -- The author's own link. There is nothing to copy: they have the set, and a
    -- second copy of it under another name is not what following your own link
    -- is asking for.
    Nothing -> invalidArgs ["That set is already yours"]
    Just found -> pure found

  cards <- setCards setId
  when (null cards) $ invalidArgs ["There is nothing in that set yet"]
  now <- liftIO getCurrentTime

  {- The copy to replace, which the access row names once they have taken it
  once. Asked of the set itself when it does not, because they may hold a copy
  from the marketplace instead -- the author published it, they subscribed, and
  the link arrived afterwards. A card code is unique per account, so writing a
  second copy would only move the cards out of the first and leave it empty. -}
  existing <-
    maybe (myCopyOfSet userId setId) (pure . Just)
      $ arkhamCardSetShareAccessCustomCardSetId accessRow

  -- What the set declares, read back off the cards rather than copied from the
  -- author's row: every card carries a stamp of it, which is the same place an
  -- import of a file or a published version reads it from.
  let (tokens, keywords) = setDeclarationsFromCards cards
  -- The author's name for it, unless another set of theirs already holds it --
  -- the copy being replaced excepted, since the name it holds is its own.
  name <- freeName userId existing (arkhamCustomCardSetName set)

  {- Sourced as nothing: `sourceCode` is the pack id an arkham.build import was
  matched by, and a copy of somebody's set is not a pack. The share is what
  identifies this copy, and that is the access row. -}
  let incoming =
        ArkhamCustomCardSet
          userId
          name
          (arkhamCustomCardSetDescription set)
          (arkhamCustomCardSetUrl set)
          Nothing
          (arkhamCustomCardSetInvestigatorExpansion set)
          (arkhamCustomCardSetIcon set)
          tokens
          keywords
          now
          now
  prepared <- traverse (prepareCardForSet userId incoming) cards

  copyId <- runDB do
    copyId <- case existing of
      Just found -> do
        P.update
          found
          [ ArkhamCustomCardSetName P.=. name
          , ArkhamCustomCardSetDescription P.=. arkhamCustomCardSetDescription set
          , ArkhamCustomCardSetUrl P.=. arkhamCustomCardSetUrl set
          , ArkhamCustomCardSetInvestigatorExpansion
              P.=. arkhamCustomCardSetInvestigatorExpansion set
          , ArkhamCustomCardSetIcon P.=. arkhamCustomCardSetIcon set
          , ArkhamCustomCardSetTokens P.=. tokens
          , ArkhamCustomCardSetKeywords P.=. keywords
          , ArkhamCustomCardSetUpdatedAt P.=. now
          ]
        P.deleteWhere [ArkhamCustomCardCustomCardSetId P.==. found]
        pure found
      Nothing -> P.insert incoming
    for_ prepared $ persistCard userId copyId now
    -- Written after the cards, so a failed copy leaves nothing claiming to hold
    -- the author's set.
    followOnly FromShare copyId
    P.update
      accessId
      [ ArkhamCardSetShareAccessCustomCardSetId P.=. Just copyId
      , ArkhamCardSetShareAccessSubscribed P.=. True
      , ArkhamCardSetShareAccessTakenAt P.=. Just now
      , ArkhamCardSetShareAccessUpdatedAt P.=. now
      ]
    pure copyId

  sharedResponse userId setId set cards
    . Just
    . Entity accessId
    $ accessRow
      { arkhamCardSetShareAccessCustomCardSetId = Just copyId
      , arkhamCardSetShareAccessSubscribed = True
      , arkhamCardSetShareAccessTakenAt = Just now
      , arkhamCardSetShareAccessUpdatedAt = now
      }

{- | What the person holding the link is told.

@behind@ is the author's set against the moment the copy was taken: their set
being newer means there is something to take. Strictly newer, because a copy is
written with the same timestamp it records -- taking it and finding yourself
behind the same instant would be a button that never goes away.
-}
sharedResponse
  :: UserId
  -> ArkhamCustomCardSetId
  -> ArkhamCustomCardSet
  -> [CustomCard]
  -> Maybe (Entity ArkhamCardSetShareAccess)
  -> Handler SharedCardSetResponse
sharedResponse userId setId set cards access = do
  author <- runDB $ DB.get (arkhamCustomCardSetUserId set)
  -- Only an approved one: a listing waiting for review is not somewhere anybody
  -- can be sent, and its author is the only person who can see it at all.
  listing <-
    runDB
      $ P.selectFirst
        [ ArkhamPublishedCardSetCustomCardSetId P.==. Just setId
        , ArkhamPublishedCardSetApprovedVersion P.!=. Nothing
        ]
        []
  let takenAt = arkhamCardSetShareAccessTakenAt . entityVal =<< access
      copyId = copyOf =<< access
  pure
    $ SharedCardSetResponse
      { sharedCardSetResponseName = arkhamCustomCardSetName set
      , sharedCardSetResponseDescription = arkhamCustomCardSetDescription set
      , sharedCardSetResponseUrl = arkhamCustomCardSetUrl set
      , sharedCardSetResponseIcon = arkhamCustomCardSetIcon set
      , sharedCardSetResponseInvestigatorExpansion =
          arkhamCustomCardSetInvestigatorExpansion set
      , sharedCardSetResponseAuthor = maybe "someone" userUsername author
      , sharedCardSetResponseMine = arkhamCustomCardSetUserId set == userId
      , sharedCardSetResponseCards = cards
      , sharedCardSetResponseUpdatedAt = arkhamCustomCardSetUpdatedAt set
      , sharedCardSetResponseListing = entityKey <$> listing
      , sharedCardSetResponseSetId = copyId
      , sharedCardSetResponseSubscribed =
          maybe False (arkhamCardSetShareAccessSubscribed . entityVal) access
      , sharedCardSetResponseBehind =
          maybe False (< arkhamCustomCardSetUpdatedAt set) takenAt
      }
 where
  copyOf = arkhamCardSetShareAccessCustomCardSetId . entityVal
