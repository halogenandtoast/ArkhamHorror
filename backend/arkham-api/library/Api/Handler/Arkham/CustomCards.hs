module Api.Handler.Arkham.CustomCards (
  getApiV1ArkhamCustomCardsR,
  postApiV1ArkhamCustomCardsR,
  postApiV1ArkhamCustomCardsArtR,
  deleteApiV1ArkhamCustomCardR,
  registerUserCustomCards,
  userCustomCards,
  ownedCardSet,
  prepareCardForSet,
  persistCard,
  saveCardInSet,
  stampSet,
  touchSet,
  unsubscribeSet,
  freeName,
  normalizeUrl,
  printedOrder,
  hostArt,
  setDeclarationsFromCards,
) where

import Amazonka
import Amazonka.S3
import Amazonka.S3.PutObject (putObject_acl, putObject_contentType)
import Control.Lens ((?~))

import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef (CardDef, cdArt, cdCardCode, cdMeta)
import Arkham.Card.CustomCard (
  CustomCard (..),
  customCardCodes,
  isCustomCardCode,
  registerCustomCards,
  sanitizeCustomCardCode,
 )
import Arkham.Custom.SetDefs qualified as SetDefs
import Auth.ApiKey qualified as ApiKey
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe, withObject)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist.Sql (SqlPersistT)
import Import hiding ((==.))
import Import qualified as P
import System.Directory (createDirectoryIfMissing)
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

{- | Stop a set following the published set it came from.

Called wherever the set's contents change. A subscription says "this is what was
published"; once a card in it has been touched that is no longer true, so the row
goes. Taking the published version again is how you get back to it.

It lives here rather than with the marketplace handlers because those import this
module, and the card writes that have to call it are here.
-}
unsubscribeSet :: ArkhamCustomCardSetId -> SqlPersistT Handler ()
unsubscribeSet setId = P.deleteWhere [ArkhamCardSetSubscriptionCustomCardSetId P.==. setId]

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

{- | Everything a card needs doing to it before it can be written: the code
checked, what its set declares stamped on, and any inlined art uploaded.

Split from the write because uploading is the part that can fail, and an import
empties a set before it refills it. Preparing every card first means a card that
cannot be stored fails while the set it is replacing is still whole.
-}
prepareCardForSet :: UserId -> ArkhamCustomCardSet -> CustomCard -> Handler (CardCode, CustomCard)
prepareCardForSet userId set card0 = do
  (cardCode, card') <- normalizeCard card0
  art <- traverse (hostArt userId) (customCardArt card')
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
  now <- liftIO getCurrentTime
  saveCardInSet userId saveCardPostSetId set now saveCardPostCard

deleteApiV1ArkhamCustomCardR :: ArkhamCustomCardId -> Handler ()
deleteApiV1ArkhamCustomCardR cardId = do
  userId <- callerUserId <$> getScopedCaller [ApiKey.cardsWrite]
  now <- liftIO getCurrentTime
  runDB do
    row <- get404 cardId
    when (arkhamCustomCardUserId row /= userId) $ lift $ permissionDenied "Not your card"
    unsubscribeSet (arkhamCustomCardCustomCardSetId row)
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

{- | The images a def carries besides its face.

An investigator has a card back and two portraits, and they live in meta rather
than on the card, so an export that only inlined the face would import with the
portraits still pointing at the exporter's library.
-}
artMetaKeys :: [Text]
artMetaKeys = ["backArt", "portrait", "portraitBack"]

hostDefArt :: UserId -> CardDef -> Handler CardDef
hostDefArt userId def = do
  meta <- foldlM host (cdMeta def) artMetaKeys
  pure def {cdMeta = meta}
 where
  host m k = case Map.lookup k m of
    Just (String v) -> do
      hosted <- hostArt userId v
      pure $ Map.insert k (String hosted) m
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
