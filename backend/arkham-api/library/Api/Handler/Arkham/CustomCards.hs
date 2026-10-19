module Api.Handler.Arkham.CustomCards (
  getApiV1ArkhamCustomCardsR,
  getApiV1ArkhamCustomCardPortraitsR,
  postApiV1ArkhamCustomCardsR,
  postApiV1ArkhamCustomCardsArtR,
  deleteApiV1ArkhamCustomCardR,
  forRendering,
  setPayloadEtag,
  registerUserCustomCards,
  userCustomCards,
  ownedCardSet,
  prepareCardForSet,
  persistCard,
  saveCardInSet,
  stampSet,
  requireUnlocked,
  freeName,
  myCopyOfSet,
  myCopyOfListing,
  followOnly,
  Source (..),
  normalizeUrl,
  printedOrder,
  hostArt,
  portableArt,
  setDeclarationsFromCards,
) where

import Amazonka
import Amazonka.S3
import Amazonka.S3.PutObject (putObject_acl, putObject_contentType)
import Control.Lens ((?~))

import Arkham.Card.CardCode (CardCode (..), unCardCode)
import Arkham.Card.CardDef (CardDef, cdArt, cdCardCode, cdMeta, cdName)
import Arkham.Card.CustomCard (
  CustomCard (..),
  customCardCodes,
  isCustomCardCode,
  registerCustomCards,
  sanitizeCustomCardCode,
 )
import Arkham.Custom.SetDefs qualified as SetDefs
import Arkham.Name (toTitle)
import Auth.ApiKey qualified as ApiKey
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe, withObject)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable (for)
import Database.Persist qualified as DB
import Database.Persist.Sql (Single (..), SqlPersistT, rawSql)
import Import hiding ((==.))
import Import qualified as P
import System.Directory (createDirectoryIfMissing)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (catch)

{- | The requesting user's card library.

A card is identified by its card code, which is minted once when the card is
first built and is what every game refers to it by. Saving is therefore an
upsert on (user, card code): editing a card replaces its row instead of leaving
a second copy behind.
-}
getApiV1ArkhamCustomCardsR :: Handler [Entity ArkhamCustomCard]
getApiV1ArkhamCustomCardsR = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsRead]
  runDB $ P.selectList [ArkhamCustomCardUserId P.==. userId] [P.Desc ArkhamCustomCardUpdatedAt]

{- | Portraits for the custom investigators named in @codes@.

What a list of game rows needs, and the only thing it needs: a row draws a face
and a name, and it was buying every card in the reader's library -- defs,
script and all -- once per row to do it.

Resolved from the reader's own library first, which is cached in process against
'libraryStamp', so the ordinary case costs nothing but a hash. A code it does
not hold belongs to somebody else's custom investigator sharing a game, and only
then is the recorded copy on the games read.
-}
getApiV1ArkhamCustomCardPortraitsR :: Handler [CardPortrait]
getApiV1ArkhamCustomCardPortraitsR = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsRead]
  codes <-
    map (CardCode . bareCustomCode)
      . filter (not . T.null)
      . T.splitOn ","
      . fromMaybe ""
      <$> lookupGetParam "codes"
  library <- userCustomCards userId
  let
    fromLibrary = [portraitOf code card | code <- codes, Just card <- [Map.lookup code library]]
    missing = [code | code <- codes, isNothing (Map.lookup code library)]
  fromGames <- if null missing then pure [] else recordedPortraits userId missing
  let payload = fromLibrary <> fromGames
  setPayloadEtag payload
  pure payload

{- | The portraits a reader's games have recorded, for cards that are not theirs.

A custom card is written onto the game that uses it, which is what lets a table
draw a card whose author is somebody else. Read through @gameCustomCards@ rather
than by loading the games: that key is a few hundred bytes beside a column that
averages 97kB, and decoding the games to reach it was the old cost of this whole
request.

@DISTINCT ON@ because the same card is recorded on every game it appears in, and
any copy answers the question.
-}
recordedPortraits :: UserId -> [CardCode] -> Handler [CardPortrait]
recordedPortraits userId codes = do
  rows :: [(Single Text, Single Value)] <-
    runDB
      $ rawSql
        "SELECT DISTINCT ON (e.key) e.key, e.value \
        \FROM arkham_games g \
        \CROSS JOIN LATERAL jsonb_each(g.current_data -> 'gameCustomCards') AS e(key, value) \
        \WHERE EXISTS (SELECT 1 FROM arkham_players p WHERE p.arkham_game_id = g.id AND p.user_id = ?) \
        \  AND e.key = ANY(string_to_array(?, ','))"
        [ toPersistValue userId
        , -- A jsonb map of 'CardCode' is keyed the way 'ToJSONKey' writes it,
          -- which is with the same leading @c@ 'bareCustomCode' takes off.
          toPersistValue (T.intercalate "," (map (T.cons 'c' . unCardCode) codes))
        ]
  pure do
    (Single code, Single raw) <- rows
    card <- maybeToList $ parseMaybe parseJSON raw
    pure $ portraitOf (CardCode (bareCustomCode code)) card

{- | Refuses anything that is not a custom card code, so this cannot be used to
shadow a real card's def.
-}
normalizeCard :: CustomCard -> Handler (CardCode, CustomCard)
normalizeCard card = do
  let cardCode = sanitizeCustomCardCode (cdCardCode (customCardDef card))
  unless (isCustomCardCode cardCode) $ invalidArgs ["Not a custom card code"]
  let def = (customCardDef card) {cdCardCode = cardCode, cdArt = unCardCode cardCode}
  pure (cardCode, card {customCardDef = def})

{- | The set at this id, if it is the requesting user's.

Lives here rather than beside the set handlers because saving a card is what
most needs it, and because the set handlers already depend on this module --
the other direction would be a cycle.
-}

{- | Whether this set follows somebody else's, and so cannot be edited.

A set in a collection is one of two things. One is your own: you can do anything
to it. The other is a copy that follows a source -- a marketplace listing, or a
link somebody sent you -- which you can read, deckbuild with, play, and bring up
to date, and not change. Changing it would make it no longer what it follows.

The old rule was the other way round: editing a followed set quietly stopped it
following. That lost the author's updates over a typo, and it never fired for the
commonest edit of all, since saving a card goes through a path that did not know
to do it -- so a "subscribed" set could drift into being something else while
still claiming to be what was published. Refusing the edit says the same thing
and can be acted on: "Add to my editor" is one button, and then the set is yours.

Both sources are asked, because a set can follow either. Neither counts once it
is out of reach: an author who removes you, stops sharing, or unlists the set has
left you with a copy that can never catch up, and a lock whose point is fidelity
to a source means nothing without the source.

It lives here rather than with the marketplace or share handlers because those
import this module, and the writes that have to refuse are here.
-}
lockedSet :: ArkhamCustomCardSetId -> Handler Bool
lockedSet setId = do
  listing <- runDB $ P.getBy (UniqueCardSetSubscription setId)
  case listing of
    Just (Entity _ row) | arkhamCardSetSubscriptionSubscribed row -> pure True
    _ ->
      runDB
        $ isJust
        <$> P.selectFirst
          [ ArkhamCardSetShareAccessCustomCardSetId P.==. Just setId
          , ArkhamCardSetShareAccessSubscribed P.==. True
          , ArkhamCardSetShareAccessRevokedAt P.==. Nothing
          ]
          []

{- | Refuse to change a set that follows somebody else's.

A 403 rather than a quiet no-op: the client hides these controls on a locked set,
so anything that reaches here is a stale page or a script, and both are better
told.
-}
requireUnlocked :: ArkhamCustomCardSetId -> Handler ()
requireUnlocked setId = do
  locked <- lockedSet setId
  when locked
    $ permissionDenied
      "That set follows someone else's. Add it to your editor to make it yours."

ownedCardSet :: UserId -> ArkhamCustomCardSetId -> Handler ArkhamCustomCardSet
ownedCardSet userId setId = do
  set <- runDB $ get404 setId
  unless (arkhamCustomCardSetUserId set == userId) $ permissionDenied "Not your card set"
  pure set

{- | Say the set changed, for a change that happened to a card in it.

A set's @updatedAt@ is read as "when this set last changed": it is what orders
the builder's list by recency, and it is the whole of how someone holding a
share of the set is told there is something new to take. A card being saved or
thrown away is the set changing, and nothing else moves the set's own row when
it happens, so these two writes have to.

Not folded into 'persistCard': an import writes the set row itself and then
every card through that, and this would be another hundred and fifty updates
saying what the first one said.
-}
touchSet :: ArkhamCustomCardSetId -> UTCTime -> SqlPersistT Handler ()
touchSet setId now = P.update setId [ArkhamCustomCardSetUpdatedAt P.=. now]

{- | The caller's own copy of somebody else's set, however they came by it.

A copy can arrive two ways -- from a link, or from the marketplace -- and either
can arrive second. A card code is unique per account, so the second copy cannot
sit beside the first: writing it moves the cards out of the one already here and
leaves an empty set behind. So "have I got this already?" has to ask both routes,
and the write that follows replaces what it finds rather than adding to it.

Asked by the author's own set, because that is what both routes point at: a share
hangs off it, and a listing names it. A listing whose author deleted their set
names nothing, and the caller is left to be found by their subscription instead
(see 'Api.Handler.Arkham.PublishedCardSets.myCopyOf').
-}
myCopyOfSet :: UserId -> ArkhamCustomCardSetId -> Handler (Maybe ArkhamCustomCardSetId)
myCopyOfSet userId sourceId = do
  viaShare <- do
    shares <- runDB $ P.selectList [ArkhamCardSetShareCustomCardSetId P.==. sourceId] []
    copies <- for shares \(Entity shareId _) -> do
      access <- runDB $ P.getBy (UniqueCardSetShareAccess shareId userId)
      pure $ arkhamCardSetShareAccessCustomCardSetId . entityVal =<< access
    pure $ listToMaybe (catMaybes copies)
  case viaShare of
    Just found -> pure (Just found)
    Nothing -> do
      listings <-
        runDB $ P.selectList [ArkhamPublishedCardSetCustomCardSetId P.==. Just sourceId] []
      copies <- for listings \(Entity publishedId _) -> myCopyOfListing userId publishedId
      pure $ listToMaybe (catMaybes copies)

{- | The caller's own copy of a listing, found by the row that records it.

Every subscription to the listing has to be looked at rather than the first one:
on a set other people have taken, the first row found is somebody else's, and
stopping there would report the caller as having no copy and make them a second.

Whether it still follows the listing or not -- the row is also "this set of mine
is my copy of that", which is as true of a copy taken into the editor.
-}
myCopyOfListing :: UserId -> ArkhamPublishedCardSetId -> Handler (Maybe ArkhamCustomCardSetId)
myCopyOfListing userId publishedId = do
  subs <-
    runDB $ P.selectList [ArkhamCardSetSubscriptionPublishedCardSetId P.==. publishedId] []
  ours <- for subs \(Entity _ sub) -> do
    let setId = arkhamCardSetSubscriptionCustomCardSetId sub
    owner <- runDB $ DB.get setId
    pure $ case owner of
      Just set | arkhamCustomCardSetUserId set == userId -> Just setId
      _ -> Nothing
  pure $ listToMaybe (catMaybes ours)

{- | Say this set no longer follows whatever it used to, bar the one route that
is taking it over.

One source at a time: a copy that both a link and a listing claim is a set
answering to two things that can disagree about what is in it. Taking it either
way therefore silences the other -- the row itself stays, because it is also the
record of which set of yours is your copy of that.
-}
followOnly :: Source -> ArkhamCustomCardSetId -> SqlPersistT Handler ()
followOnly source setId = do
  when (source /= FromShare)
    $ P.updateWhere
      [ArkhamCardSetShareAccessCustomCardSetId P.==. Just setId]
      [ArkhamCardSetShareAccessSubscribed P.=. False]
  when (source /= FromListing)
    $ P.updateWhere
      [ArkhamCardSetSubscriptionCustomCardSetId P.==. setId]
      [ArkhamCardSetSubscriptionSubscribed P.=. False]

-- | Which of the two routes a copy is being taken through.
data Source = FromShare | FromListing
  deriving stock Eq

{- | A name no set of this user's holds, bar the one being written.

Taking a copy of something called what one of your own sets is called should not
fail, and should not silently replace it. Lives here rather than with either
importer because both the marketplace and a share link need it.

The set being written is excepted because a copy taken /again/ is renamed to
whatever the original is called now, and the name it already holds is its own: a
copy keeping its name is not a clash, while a third set of yours holding that
name still is -- which is a rename straight into a unique index, and used to be
a 500 on the way to catching up with the author.
-}
freeName :: UserId -> Maybe ArkhamCustomCardSetId -> Text -> Handler Text
freeName userId mine name = go name (2 :: Int)
 where
  go candidate attempt = do
    taken <- runDB $ P.getBy (UniqueUserCustomCardSetName userId candidate)
    case taken of
      Nothing -> pure candidate
      Just (Entity found _) | Just found == mine -> pure candidate
      Just _
        | attempt > 50 -> invalidArgs ["You already have a set called \"" <> name <> "\""]
        | otherwise -> go (name <> " (" <> tshow attempt <> ")") (attempt + 1)

{- | A link as it is stored: blank is nothing, trimmed, and an @http@ address or
a refusal. The scheme is checked here rather than wherever it is shown, because
what comes out of this column goes into an anchor someone clicks, and a
@javascript:@ "address" in that position is not a link.

Shared by the set and its listing, which hold the same link and have to agree on
what one is.
-}
normalizeUrl :: Maybe Text -> Handler (Maybe Text)
normalizeUrl raw = case T.strip <$> raw of
  Nothing -> pure Nothing
  Just text
    | T.null text -> pure Nothing
    | any (`T.isPrefixOf` T.toLower text) ["http://", "https://"] -> pure (Just text)
    | otherwise -> invalidArgs ["A set's link has to start with http:// or https://"]

{- | What the set it belongs to writes into a card's own def: its name, and --
for an investigator expansion -- the tokens and keywords the set brings with it.

The set row is what a card belongs to. These copies travel with the card, and
are what answer for the set when a card arrives somewhere there are no set rows
to read: a card exported on its own and imported into another account, the
pre-set libraries the backfill migration filed by the name field, and a card in
a running game, where the engine and the client have the def and nothing else.
The keywords are read there to resolve what a card's @_keywords@ refers to, and
the tokens to draw a marker the engine only knows the slug of.

An ordinary set writes nothing but its name: an empty list is nothing to say, so
the key goes rather than being written onto every card of every set that has no
tokens.
-}
stampSet :: ArkhamCustomCardSet -> CardDef -> CardDef
stampSet set def = def {cdMeta = stamped}
 where
  stamped =
    Map.insert "set" (String (arkhamCustomCardSetName set))
      $ declared SetDefs.setTokensMetaKey (arkhamCustomCardSetTokens set)
      $ declared SetDefs.setKeywordsMetaKey (arkhamCustomCardSetKeywords set)
      $ cdMeta def
  declared key value meta = case value of
    Array xs | null xs -> Map.delete key meta
    Null -> Map.delete key meta
    _ -> Map.insert key value meta

{- | The tokens and keywords a set declared, read back off the cards it was
published or exported with.

Every card carries a copy ('stampSet'), so what a set brings with it survives
anywhere a card goes -- which means an import does not need the set's own row,
or a published version to have been written in a format that had room for it.
The first card that says anything answers for the set; they are all stamped from
the same row, so any of them would.
-}
setDeclarationsFromCards :: [CustomCard] -> (Value, Value)
setDeclarationsFromCards cards = (declared SetDefs.setTokensMetaKey, declared SetDefs.setKeywordsMetaKey)
 where
  declared key =
    fromMaybe (Array mempty)
      $ listToMaybe [v | card <- cards, Just v <- [Map.lookup key (cdMeta (customCardDef card))]]

{- | A card's place in its set: the number printed on it, read out of its own def.

Sorted on the leading digits rather than the text, so 10 follows 9. A card with no
number sorts last, since it has said nothing about where it goes.
-}
printedOrder :: Value -> (Int, Text)
printedOrder value = (fromMaybe maxBound (readMaybe (T.unpack digits)), number)
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

{- | Everything a card needs doing to it before it can be written: the code
checked, what its set declares stamped on, and any inlined art uploaded.

Split from the write because uploading is the part that can fail, and an import
empties a set before it refills it. Preparing every card first means a card that
cannot be stored fails while the set it is replacing is still whole.
-}
prepareCardForSet :: UserId -> ArkhamCustomCardSet -> CustomCard -> Handler (CardCode, CustomCard)
prepareCardForSet userId set card0 = do
  (cardCode, card') <- normalizeCard card0
  art <- join <$> traverse (portableArt userId) (customCardArt card')
  def <- hostDefArt userId (stampSet set (customCardDef card'))
  pure (cardCode, card' {customCardArt = art, customCardDef = def})

{- | Write a prepared card into a set. Runs in the caller's transaction, so an
import can empty a set and refill it as one thing.

Spelled out rather than written as @DB@: that alias hides a @forall m@, and a
function whose result is still polymorphic cannot be handed to 'traverse', which
is how an import writes a whole set of these.
-}
persistCard
  :: UserId
  -> ArkhamCustomCardSetId
  -> UTCTime
  -> (CardCode, CustomCard)
  -> SqlPersistT Handler (Entity ArkhamCustomCard)
persistCard userId setId now (cardCode, card) = do
  let row =
        ArkhamCustomCard
          userId
          setId
          (unCardCode cardCode)
          (toJSON (customCardDef card))
          (customCardArt card)
          now
          now
  P.getBy (UniqueUserCustomCard userId (unCardCode cardCode)) >>= \case
    {- A card belongs to one set, and the row is keyed on its code alone, so
    writing it into a second set does not copy it there -- it moves it, and
    leaves the set it came from one card short without saying so. That is never
    what an import means: a file whose cards are already somewhere else is a
    file pointed at the wrong set. -}
    Just (Entity _ existing)
      | arkhamCustomCardCustomCardSetId existing /= setId -> do
          -- `P` here is this project's prelude, whose `get` is MonadState's.
          home <- P.getEntity (arkhamCustomCardCustomCardSetId existing)
          invalidArgs
            [ toTitle (cdName (customCardDef card))
                <> " is already in "
                <> maybe "another set" (arkhamCustomCardSetName . entityVal) home
                <> ". A card belongs to one set, so importing it here would take it out of that one."
            ]
    Just (Entity rowId existing) -> do
      P.update
        rowId
        [ ArkhamCustomCardCustomCardSetId P.=. setId
        , ArkhamCustomCardDef P.=. toJSON (customCardDef card)
        , ArkhamCustomCardArt P.=. customCardArt card
        , ArkhamCustomCardUpdatedAt P.=. now
        ]
      -- Built from the existing row so createdAt survives an edit.
      pure
        $ Entity rowId
        $ existing
          { arkhamCustomCardCustomCardSetId = setId
          , arkhamCustomCardDef = toJSON (customCardDef card)
          , arkhamCustomCardArt = customCardArt card
          , arkhamCustomCardUpdatedAt = now
          }
    Nothing -> do
      rowId <- P.insert row
      pure $ Entity rowId row

saveCardInSet
  :: UserId
  -> ArkhamCustomCardSetId
  -> ArkhamCustomCardSet
  -> UTCTime
  -> CustomCard
  -> Handler (Entity ArkhamCustomCard)
saveCardInSet userId setId set now card0 = do
  prepared <- prepareCardForSet userId set card0
  runDB do
    saved <- persistCard userId setId now prepared
    touchSet setId now
    pure saved

-- | A card is always saved into a set, so the set it goes in comes with it.
data SaveCardPost = SaveCardPost
  { saveCardPostSetId :: ArkhamCustomCardSetId
  , saveCardPostCard :: CustomCard
  }

instance FromJSON SaveCardPost where
  parseJSON v =
    withObject
      "SaveCardPost"
      (\o -> SaveCardPost . ArkhamCustomCardSetKey <$> o .: "setId" <*> parseJSON v)
      v

postApiV1ArkhamCustomCardsR :: Handler (Entity ArkhamCustomCard)
postApiV1ArkhamCustomCardsR = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  SaveCardPost {saveCardPostSetId, saveCardPostCard} <- requireCheckJsonBody
  set <- ownedCardSet userId saveCardPostSetId
  requireUnlocked saveCardPostSetId
  now <- liftIO getCurrentTime
  saveCardInSet userId saveCardPostSetId set now saveCardPostCard

deleteApiV1ArkhamCustomCardR :: ArkhamCustomCardId -> Handler ()
deleteApiV1ArkhamCustomCardR cardId = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  row <- runDB $ get404 cardId
  when (arkhamCustomCardUserId row /= userId) $ permissionDenied "Not your card"
  requireUnlocked (arkhamCustomCardCustomCardSetId row)
  now <- liftIO getCurrentTime
  runDB do
    P.delete cardId
    touchSet (arkhamCustomCardCustomCardSetId row) now

{- | Where a custom card's art lives.

Kept under its own prefix so it never mingles with the card images the asset
pipeline syncs, and named by the hash of its contents so uploading the same
image twice costs nothing and two cards can share art without one edit
disturbing the other.
-}
artBucket :: BucketName
artBucket = "arkham-horror-assets"

artPrefix :: Text
artPrefix = "img/custom/"

maxArtBytes :: Int64
maxArtBytes = 1024 * 1024

extensionFor :: Maybe Text -> Text
extensionFor = \case
  Just "image/png" -> "png"
  Just "image/jpeg" -> "jpg"
  Just "image/gif" -> "gif"
  Just "image/avif" -> "avif"
  _ -> "webp"

{- | Where a user's art lives.

Scoped by user: the object name is a content hash, so without a per-user prefix
two people who upload the same bytes share an object, and anyone able to produce
those bytes could write over it. The prefix keeps each library's images its own.
-}
artPrefixFor :: UserId -> Text
artPrefixFor userId = artPrefix <> toPathPiece userId <> "/"

-- | Put image bytes where they are served from, and say where that is.
storeArt :: UserId -> Text -> BSL.ByteString -> Handler Text
storeArt userId contentType bytes = do
  unless ("image/" `T.isPrefixOf` contentType) $ invalidArgs ["Not an image"]
  -- The browser downscales before uploading; this is the backstop.
  when (BSL.length bytes > maxArtBytes) $ invalidArgs ["Image is larger than 1MB"]

  let
    prefix = artPrefixFor userId
    filename =
      decodeUtf8 (B16.encode $ SHA256.hashlazy bytes) <> "." <> extensionFor (Just contentType)
    key = ObjectKey $ prefix <> filename

  getsApp (appCustomCardArtDir . appSettings) >>= \case
    -- Development: keep art on disk in the frontend's public directory, so
    -- testing never writes to the bucket everyone's images are served from.
    Just dir -> liftIO do
      let userDir = dir <> "/" <> T.unpack (toPathPiece userId)
      createDirectoryIfMissing True userDir
      BSL.writeFile (userDir <> "/" <> T.unpack filename) bytes
      pure $ "/" <> prefix <> filename
    Nothing -> do
      liftIO do
        env <- newEnv discover
        runResourceT do
          mExisting <-
            catch @_ @Error (Just <$> send env (newHeadObject artBucket key)) (\_ -> pure Nothing)
          whenNothing_ mExisting do
            void
              . send env
              $ newPutObject artBucket key (toBody bytes)
              & (putObject_acl ?~ ObjectCannedACL_Public_read)
              & (putObject_contentType ?~ contentType)

      assetHost <- getsApp (appAssetHost . appSettings)
      pure $ fromMaybe "https://assets.arkhamhorror.app" assetHost <> "/" <> prefix <> filename

postApiV1ArkhamCustomCardsArtR :: Handler Text
postApiV1ArkhamCustomCardsArtR = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  (_, files) <- runRequestBody
  file <- case files of
    (_, f) : _ -> pure f
    [] -> invalidArgs ["No image uploaded"]
  bytes <- BSL.fromStrict <$> fileSourceByteString file
  storeArt userId (fileContentType file) bytes

{- | An exported card carries its image inline, so an import has something to
store rather than a link into the exporter's library.

Art that is already a URL is left alone; a @data:@ URI is decoded and stored
under the importing user, and the card keeps the hosted address.
-}
hostArt :: UserId -> Text -> Handler Text
hostArt userId art = case parseDataUri art of
  Nothing -> pure art
  Just (contentType, bytes) -> storeArt userId contentType bytes

{- | Art as it can be stored here, if it can be at all.

Art lives under the host of the library it was uploaded to. In production that
is an asset host, so the address means the same thing to everyone. In
development 'storeArt' writes to the frontend's public directory and answers
with a bare path, which names a file on the machine it was uploaded on and
nothing at all anywhere else.

Such a path can arrive here: an export falls back to the URL when it cannot read
the image, and a set's tokens are read back off the copy stamped on each card
when the file names no set -- a loose card, or a selection out of the builder.
Stored as given, it is an image that resolves to the single-page app on every
request, which renders as broken art with nothing to say why.

So it is dropped instead, and the token draws as a plain marker while the
builder shows an empty image control -- which is true, and is one upload to put
right. Only a path into a library is judged, and only where this server is not
the one serving it; anything else is somebody else's address to keep.
-}
portableArt :: UserId -> Text -> Handler (Maybe Text)
portableArt userId art = do
  hosted <- hostArt userId art
  servesLocally <- isJust <$> getsApp (appCustomCardArtDir . appSettings)
  pure do
    guard . not $ T.null (T.strip hosted)
    guard $ servesLocally || not (("/" <> artPrefix) `T.isPrefixOf` hosted)
    pure hosted

{- | The images a def carries besides its face.

An investigator has a card back and two portraits, and they live in meta rather
than on the card, so an export that only inlined the face would import with the
portraits still pointing at the exporter's library.
-}
artMetaKeys :: [Text]
artMetaKeys = ["backArt", "portrait", "portraitBack"]

-- ------------------------------------------------------- what a reader gets ---

{- | The meta keys that say what a card /does/.

These are the card builder's authoring surface, and the engine is the only thing
that reads them: it interprets them server-side out of the library, so nothing
outside the builder has ever needed them in a response. On a scripted card they
are most of the def.

@_locale@ and @_signatures@ are deliberately not here. The first carries the
card's text, which every player at the table has to be able to read, and the
second is how a deck resolves a signature card to its owner.
-}
dslMetaKeys :: [Text]
dslMetaKeys =
  [ "_abilities"
  , "_handlers"
  , "_modifiers"
  , "_onPlay"
  , "_onSetup"
  , "_onRevelation"
  , "_revelationPlacement"
  , "_elderSign"
  , "_elderSignSteps"
  , "_elderSignRevealSteps"
  , "_elderSignSuccessSteps"
  , "_elderSignFailureSteps"
  , "_setAside"
  , "_keywords"
  ]

{- | A card as something other than the builder needs it: the printed face, its
text and its signatures, and none of the script behind it.

The registry this lands in is read to draw cards and to name them; the behaviour
it used to carry was being sent to every player of every game on every page
load, for nobody to read.
-}
forRendering :: CustomCard -> CustomCard
forRendering card = card {customCardDef = def {cdMeta = foldr Map.delete (cdMeta def) dslMetaKeys}}
 where
  def = customCardDef card

{- | What a game row needs of a custom investigator, which is its portrait and
nothing else.

'Api.Handler.Arkham.Games.Summary' names the investigators in a list of games
but cannot draw their faces: a custom investigator's portrait lives on the def,
in the author's library. This is that lookup, and it is the whole of it -- four
fields against a def that can run to kilobytes of script.
-}
data CardPortrait = CardPortrait
  { cardCode :: CardCode
  , art :: Maybe Text
  , portrait :: Maybe Text
  , portraitBack :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

{- | A custom card code as the library is keyed by it.

@ToJSON CardCode@ prepends a @c@ so a code never serializes as a bare number, so
an id a reader is asking about comes back as @c*…@ while the card is stored
under @*…@. The frontend normalises the same way round (@stripCardCodePrefix@),
and the two have to agree or every lookup here misses.
-}
bareCustomCode :: Text -> Text
bareCustomCode raw = maybe stripped ("*" <>) (T.stripPrefix "c*" stripped)
 where
  stripped = T.strip raw

portraitOf :: CardCode -> CustomCard -> CardPortrait
portraitOf code card =
  CardPortrait
    { cardCode = code
    , art = customCardArt card
    , portrait = metaText "portrait"
    , portraitBack = metaText "portraitBack"
    }
 where
  metaText k = case Map.lookup k (cdMeta (customCardDef card)) of
    Just (String t) -> Just t
    _ -> Nothing

{- | Tag a response so a reader that already has it is told so.

Two things have to be true together for that to happen, which is why they are
set in one place: an @ETag@ to compare, and a @Cache-Control@ that lets the
browser keep the response long enough to compare it. The global CORS headers say
@no-store@, which forbids keeping anything at all; 'addCORSHeaders' leaves
'Cache-Control' alone when a handler has set its own, so this wins for these
endpoints without changing what every other endpoint promises.

@private@ because a library belongs to one account, and @no-cache@ rather than a
lifetime because an edit in the builder has to show at the table immediately --
revalidate every time, and pay for the body only when it actually changed.

'setEtag' answers 304 itself when the reader's @If-None-Match@ matches.
-}
setPayloadEtag :: ToJSON a => a -> Handler ()
setPayloadEtag payload = do
  addHeader "Cache-Control" "private, no-cache"
  setEtag . decodeUtf8 . B16.encode . SHA256.hashlazy $ encode payload

hostDefArt :: UserId -> CardDef -> Handler CardDef
hostDefArt userId def = do
  meta <- foldlM host (cdMeta def) artMetaKeys
  pure def {cdMeta = meta}
 where
  host m k = case Map.lookup k m of
    Just (String v) ->
      portableArt userId v <&> \case
        Nothing -> Map.delete k m
        Just hosted -> Map.insert k (String hosted) m
    _ -> pure m

parseDataUri :: Text -> Maybe (Text, BSL.ByteString)
parseDataUri t = do
  rest <- T.stripPrefix "data:" t
  let (meta, payload) = T.breakOn "," rest
  body <- T.stripPrefix "," payload
  contentType <- T.stripSuffix ";base64" meta
  bytes <- either (const Nothing) Just $ B64.decode (encodeUtf8 body)
  pure (contentType, BSL.fromStrict bytes)

{- | Make a user's library resolvable.

Custom defs normally reach the engine through the game that uses them. A deck is
built before any of that, so anything that has to read a custom card outside a
game -- validating a decklist, loading one -- has to put the user's library into
the registry first.
-}
userCustomCards :: UserId -> Handler (Map CardCode CustomCard)
userCustomCards userId = do
  stamp <- libraryStamp userId
  remembered <- Map.lookup userId <$> readIORef userCustomCardsCache
  case remembered of
    Just (cachedStamp, cards) | cachedStamp == stamp -> pure cards
    _ -> do
      cards <- readUserCustomCards userId
      atomicModifyIORef' userCustomCardsCache \m -> (Map.insert userId (stamp, cards) m, ())
      pure cards

{- | What a cached library is checked against before it is believed.

The row count and the newest @updatedAt@ between them see every change a
library can undergo: a save moves the timestamp, an addition moves both, a
deletion lowers the count. A deletion paired with an addition holds the count
still but cannot hold the timestamp, the new row's being the moment it was
written.

Two indexed aggregates, against reading and reparsing the whole library. That
read was happening on every websocket answer -- the freshness it buys is the
point (a card edited in the builder has to show at a table already using it),
but the author's library is megabytes of defs and almost never differs from the
answer before.
-}
type LibraryStamp = (Int, Maybe UTCTime)

libraryStamp :: UserId -> Handler LibraryStamp
libraryStamp userId = runDB do
  n <- P.count [ArkhamCustomCardUserId P.==. userId]
  newest <- P.selectFirst [ArkhamCustomCardUserId P.==. userId] [P.Desc ArkhamCustomCardUpdatedAt]
  pure (n, arkhamCustomCardUpdatedAt . entityVal <$> newest)

{-# NOINLINE userCustomCardsCache #-}
userCustomCardsCache :: IORef (Map UserId (LibraryStamp, Map CardCode CustomCard))
userCustomCardsCache = unsafePerformIO (newIORef mempty)

readUserCustomCards :: UserId -> Handler (Map CardCode CustomCard)
readUserCustomCards userId = do
  rows <- runDB $ P.selectList [ArkhamCustomCardUserId P.==. userId] []
  let cards = do
        Entity _ row <- rows
        def <- maybeToList $ parseMaybe parseJSON (arkhamCustomCardDef row)
        pure $ CustomCard def (arkhamCustomCardArt row)
  {- Aliases underneath, so a card's own code always wins one. Importing the
  arkham.build pack gives its card a code derived from the same id a hand-built
  card can claim, and in that collision the imported card is the one the deck
  means. Left-biased union rather than row order, which is `updatedAt` and says
  nothing about which card is which. -}
  pure
    $ Map.fromList [(cdCardCode (customCardDef c), c) | c <- cards]
    `Map.union` Map.fromList [(code, c) | c <- cards, code <- drop 1 (customCardCodes c)]

registerUserCustomCards :: UserId -> Handler ()
registerUserCustomCards userId = registerCustomCards =<< userCustomCards userId
