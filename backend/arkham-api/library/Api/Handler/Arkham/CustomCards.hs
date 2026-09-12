module Api.Handler.Arkham.CustomCards (
  getApiV1ArkhamCustomCardsR,
  postApiV1ArkhamCustomCardsR,
  postApiV1ArkhamCustomCardsArtR,
  deleteApiV1ArkhamCustomCardR,
  registerUserCustomCards,
  ownedCardSet,
  prepareCardForSet,
  persistCard,
  saveCardInSet,
  stampSetName,
  unsubscribeSet,
) where

import Amazonka
import Amazonka.S3
import Amazonka.S3.PutObject (putObject_acl, putObject_contentType)
import Control.Lens ((?~))

import Arkham.Card.CardCode (CardCode (..))
import Arkham.Card.CardDef (CardDef, cdArt, cdCardCode, cdMeta)
import Arkham.Card.CustomCard (
  CustomCard (..),
  isCustomCardCode,
  registerCustomCards,
  sanitizeCustomCardCode,
 )
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson.Types (parseMaybe, withObject)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
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
  userId <- getRequestUserId
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

{- | The set's name, written into the card's own def.

The set row is what a card belongs to. This copy travels with the card, and is
what names the set when a card arrives somewhere there are no set rows to read:
a card exported on its own and imported into another account, and the pre-set
libraries the backfill migration filed by this same field. Nothing in a running
app reads it to display a set -- that comes off the row.
-}
stampSetName :: Text -> CardDef -> CardDef
stampSetName name def = def {cdMeta = Map.insert "set" (String name) (cdMeta def)}

{- | Everything a card needs doing to it before it can be written: the code
checked, the set name stamped on, and any inlined art uploaded.

Split from the write because uploading is the part that can fail, and an import
empties a set before it refills it. Preparing every card first means a card that
cannot be stored fails while the set it is replacing is still whole.
-}
prepareCardForSet :: UserId -> Text -> CustomCard -> Handler (CardCode, CustomCard)
prepareCardForSet userId setName card0 = do
  (cardCode, card') <- normalizeCard card0
  art <- traverse (hostArt userId) (customCardArt card')
  def <- hostDefArt userId (stampSetName setName (customCardDef card'))
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
  -> Text
  -> UTCTime
  -> CustomCard
  -> Handler (Entity ArkhamCustomCard)
saveCardInSet userId setId setName now card0 = do
  prepared <- prepareCardForSet userId setName card0
  runDB $ persistCard userId setId now prepared

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
  userId <- getRequestUserId
  SaveCardPost {saveCardPostSetId, saveCardPostCard} <- requireCheckJsonBody
  set <- ownedCardSet userId saveCardPostSetId
  now <- liftIO getCurrentTime
  saveCardInSet userId saveCardPostSetId (arkhamCustomCardSetName set) now saveCardPostCard

deleteApiV1ArkhamCustomCardR :: ArkhamCustomCardId -> Handler ()
deleteApiV1ArkhamCustomCardR cardId = do
  userId <- getRequestUserId
  runDB do
    row <- get404 cardId
    when (arkhamCustomCardUserId row /= userId) $ lift $ permissionDenied "Not your card"
    unsubscribeSet (arkhamCustomCardCustomCardSetId row)
    P.delete cardId

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
  userId <- getRequestUserId
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
registerUserCustomCards :: UserId -> Handler ()
registerUserCustomCards userId = do
  rows <- runDB $ P.selectList [ArkhamCustomCardUserId P.==. userId] []
  registerCustomCards $ Map.fromList do
    Entity _ row <- rows
    def <- maybeToList $ parseMaybe parseJSON (arkhamCustomCardDef row)
    pure (cdCardCode def, CustomCard def (arkhamCustomCardArt row))
