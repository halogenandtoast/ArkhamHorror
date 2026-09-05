module Api.Handler.Arkham.CustomCards (
  getApiV1ArkhamCustomCardsR,
  postApiV1ArkhamCustomCardsR,
  postApiV1ArkhamCustomCardsImportR,
  postApiV1ArkhamCustomCardsArtR,
  deleteApiV1ArkhamCustomCardR,
  registerUserCustomCards,
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
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
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

saveCard :: UserId -> UTCTime -> CustomCard -> Handler (Entity ArkhamCustomCard)
saveCard userId now card0 = do
  (cardCode, card') <- normalizeCard card0
  art <- traverse (hostArt userId) (customCardArt card')
  def <- hostDefArt userId (customCardDef card')
  let card = card' {customCardArt = art, customCardDef = def}
  let row =
        ArkhamCustomCard
          userId
          (unCardCode cardCode)
          (toJSON (customCardDef card))
          (customCardArt card)
          now
          now
  runDB do
    P.getBy (UniqueUserCustomCard userId (unCardCode cardCode)) >>= \case
      Just (Entity rowId existing) -> do
        P.update
          rowId
          [ ArkhamCustomCardDef P.=. toJSON (customCardDef card)
          , ArkhamCustomCardArt P.=. customCardArt card
          , ArkhamCustomCardUpdatedAt P.=. now
          ]
        -- Built from the existing row so createdAt survives an edit.
        pure
          $ Entity rowId
          $ existing
            { arkhamCustomCardDef = toJSON (customCardDef card)
            , arkhamCustomCardArt = customCardArt card
            , arkhamCustomCardUpdatedAt = now
            }
      Nothing -> do
        rowId <- P.insert row
        pure $ Entity rowId row

postApiV1ArkhamCustomCardsR :: Handler (Entity ArkhamCustomCard)
postApiV1ArkhamCustomCardsR = do
  userId <- getRequestUserId
  card <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  saveCard userId now card

newtype CustomCardImport = CustomCardImport {cards :: [CustomCard]}
  deriving stock Generic
  deriving anyclass FromJSON

{- | Bulk upsert, for importing an exported file. Cards keep the codes they were
exported with, so re-importing your own export updates those cards rather than
duplicating them.
-}
postApiV1ArkhamCustomCardsImportR :: Handler [Entity ArkhamCustomCard]
postApiV1ArkhamCustomCardsImportR = do
  userId <- getRequestUserId
  CustomCardImport {cards} <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  traverse (saveCard userId now) cards

deleteApiV1ArkhamCustomCardR :: ArkhamCustomCardId -> Handler ()
deleteApiV1ArkhamCustomCardR cardId = do
  userId <- getRequestUserId
  runDB do
    row <- get404 cardId
    when (arkhamCustomCardUserId row /= userId) $ lift $ permissionDenied "Not your card"
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
