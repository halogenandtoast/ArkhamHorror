module Api.Handler.Arkham.Decks (
  getApiV1ArkhamDecksR,
  getApiV1ArkhamDeckR,
  postApiV1ArkhamDecksR,
  postApiV1ArkhamDecksFetchR,
  postApiV1ArkhamDecksValidateR,
  deleteApiV1ArkhamDeckR,
  putApiV1ArkhamGameDecksR,
  postApiV1ArkhamSyncDeckR,
) where

import Import hiding (delete, on, update, (=.), (==.))

import Api.Arkham.Helpers
import Api.Handler.Arkham.Games.Shared (publishToRoom)
import Arkham.Card.CardCode
import Arkham.Classes.Entity (attr)
import Arkham.Classes.HasQueue
import Arkham.Decklist
import Arkham.Game
import Arkham.Game.Diff
import Arkham.Game.State (isChooseDecks)
import Arkham.Game.Utils (gameInvestigators)
import Arkham.Id
import Arkham.Investigator.Cards (allInvestigatorCards)
import Arkham.Investigator.Types (investigatorDeckUrl, investigatorPlayerId)
import Arkham.Message
import Arkham.PlayerCard
import Arkham.Queue
import Control.Exception (evaluate)
import Control.Lens (view)
import Control.Monad.Random (mkStdGen)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable (for)
import Database.Esqueleto.Experimental hiding (isNothing, (<&>))
import Entity.Arkham.Step
import Json hiding (Success)
import Network.HTTP.Conduit (
  httpLbs,
  newManager,
  parseRequest,
  requestHeaders,
  responseBody,
  simpleHttp,
  tlsManagerSettings,
 )
import Network.HTTP.Types
import Network.HTTP.Types.Status qualified as Status
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import UnliftIO.Exception (try)

getApiV1ArkhamDecksR :: Handler [Entity ArkhamDeck]
getApiV1ArkhamDecksR = do
  userId <- getRequestUserId
  runDB $ select do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.userId ==. val userId
    pure decks

data CreateDeckPost = CreateDeckPost
  { deckId :: Text
  , deckName :: Text
  , deckUrl :: Maybe Text
  , deckList :: ArkhamDBDecklist
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype ValidateDeckPost = ValidateDeckPost
  { validateDeckList :: ArkhamDBDecklist
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype FetchDeckPost = FetchDeckPost
  { fetchDeckUrl :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON FetchDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "fetchDeck"

data UpgradeDeckPost = UpgradeDeckPost
  { udpInvestigatorId :: InvestigatorId
  , udpDeckUrl :: Maybe Text
  , udpDeckList :: Maybe ArkhamDBDecklist
  }
  deriving stock (Show, Generic)

instance FromJSON UpgradeDeckPost where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"

newtype DeckError = UnimplementedCard CardCode
  deriving stock (Show, Eq, Generic)

instance ToJSON DeckError where
  toJSON = genericToJSON $ defaultOptions {tagSingleConstructors = True}

toDeckErrors :: ArkhamDBDecklist -> [DeckError]
toDeckErrors decklist = flip mapMaybe cardCodes \cardCode ->
  maybe
    (Just $ UnimplementedCard cardCode)
    (const Nothing)
    (Map.lookup cardCode allPlayerCards)
 where
  cardCodes = Map.keys $ slots decklist

postApiV1ArkhamDecksR :: Handler (Entity ArkhamDeck)
postApiV1ArkhamDecksR = do
  userId <- getRequestUserId
  postData <- requireCheckJsonBody
  let deck = fromPostData userId postData
  case toDeckErrors (arkhamDeckList deck) of
    [] -> runDB $ insertEntity deck
    err -> sendStatusJSON status400 err

postApiV1ArkhamDecksValidateR :: Handler ()
postApiV1ArkhamDecksValidateR = do
  _ <- getRequestUserId
  decklist <- requireCheckJsonBody
  case toDeckErrors decklist of
    [] -> sendStatusJSON status200 ()
    err -> sendStatusJSON status400 err

postApiV1ArkhamDecksFetchR :: Handler ArkhamDBDecklist
postApiV1ArkhamDecksFetchR = do
  _ <- getRequestUserId
  FetchDeckPost {..} <- requireCheckJsonBody
  getDeckList fetchDeckUrl >>= \case
    Right decklist -> pure decklist
    Left err -> sendStatusJSON Status.status400 (JSONError $ T.pack err)

{- | Load an upgraded (or replacement) deck into a running campaign.

Every failure here has to be inert: the deck is fetched and validated BEFORE the game is
touched, the engine run is wrapped so a throwing message can never be half-persisted, and
anything that goes wrong is reported as a real status code instead of a bare 500. The
reporter of #5256 hit the opposite of all three -- an upgrade that appeared to do nothing,
so they submitted it again, which re-ran the whole load on an already-upgraded deck.
-}
putApiV1ArkhamGameDecksR :: ArkhamGameId -> Handler ()
putApiV1ArkhamGameDecksR gameId = do
  userId <- getRequestUserId
  postData <- requireCheckJsonBody
  tracer <- getTracer
  now <- liftIO getCurrentTime

  -- Resolve the deck before opening the game transaction. Fetching inside it held the
  -- game's row lock across an ArkhamDB round trip, and every failure (site unreachable,
  -- malformed json, a card the engine does not implement) was a bare `error`: an opaque
  -- 500 with nothing for the player to act on.
  mDecklist <- resolveDecklist postData

  outcome <- runDB $ atomicallyWithGame gameId \gameEntity@ArkhamGame {..} -> do
    mLastStep <- getBy (UniqueStep gameId arkhamGameStep)
    let Game {..} = arkhamGameCurrentData
    let investigatorId = udpInvestigatorId postData
    let currentQueue = maybe [] (choiceMessages . arkhamStepChoice . entityVal) mLastStep

    -- A stale client (this campaign swaps which investigators exist between scenarios)
    -- would otherwise crash on getPlayer deep inside the engine.
    case Map.lookup investigatorId (gameInvestigators arkhamGameCurrentData) of
      Nothing -> pure $ Left (Status.status400, "That investigator is not in this game")
      Just investigatorEntity -> do
        let playerId = attr investigatorPlayerId investigatorEntity
        -- A seat is owed a deck exactly while its own deck question is parked: answering
        -- drops it (and re-parks only the seats still waiting). Without this check a
        -- resubmit -- the client's upgrade buttons come back as soon as the request
        -- resolves, so a player who misses the websocket update clicks again -- re-ran the
        -- load against an already-upgraded deck, duplicating every card in it and (before
        -- the runMessages guard) destroying the campaign's parked question (#5256). Game
        -- state alone is too coarse: a multiplayer upgrade window stays in IsChooseDecks
        -- until the last seat answers. Report success either way so the redundant click
        -- still re-syncs the client from the publish below.
        if
          | not (isChooseDecks gameGameState) -> pure $ Right gameEntity
          | not (maybe False isDeckQuestion $ Map.lookup playerId gameQuestion) -> pure $ Right gameEntity
          | otherwise -> do
              -- The engine can throw (an unimplemented card, an unusable decklist, an
              -- unexpected game state). Keep the whole run -- including the undo diff, which
              -- forces the resulting game -- inside the catch, so a failure leaves the row
              -- exactly as it was rather than persisting a partly-applied upgrade.
              upgraded <- liftIO $ try @_ @SomeException do
                gameRef <- newIORef arkhamGameCurrentData
                queueRef <- newQueue currentQueue
                genRef <- newIORef $ mkStdGen gameSeed
                runGameApp (GameApp gameRef queueRef genRef (pure . const ()) tracer Nothing) do
                  let question' = Map.delete playerId gameQuestion
                  unless (Map.null question') (push $ AskMap question')
                  -- No deck at all is the "continue without upgrading" path: push nothing
                  -- and let the parked continuation run.
                  for_ mDecklist \decklist ->
                    push
                      $ if sameInvestigator investigatorId decklist
                        then UpgradeDecklist investigatorId decklist
                        else ReplaceInvestigator investigatorId decklist
                  runMessages (gameIdToText gameId) Nothing
                ge <- readIORef gameRef
                updatedQueue <- readIORef (queueToRef queueRef)
                diffDown <- evaluate $ diff ge arkhamGameCurrentData
                pure (ge, updatedQueue, diffDown)

              case upgraded of
                -- First line only: engine `error`s carry a call stack the player cannot use.
                Left err ->
                  pure $ Left (Status.status500, "Could not upgrade deck: " <> T.takeWhile (/= '\n') (tshow err))
                Right (ge, updatedQueue, diffDown) -> do
                  let g' =
                        ArkhamGame
                          arkhamGameName
                          ge
                          (arkhamGameStep + 1)
                          arkhamGameMultiplayerVariant
                          arkhamGameCreatedAt
                          now

                  replace gameId g'
                  insert_
                    $ ArkhamStep
                      gameId
                      (Choice diffDown updatedQueue)
                      (arkhamGameStep + 1)
                      (ActionDiff $ view actionDiffL ge)

                  {- Keep the player's saved deck in step with the campaign. Without this the
                  deck page (which renders the arkham_decks row) shows the list as first
                  imported forever, and its Sync button cannot help: an arkham.build deck url is
                  a version-pinned share snapshot (#5257). There is no game -> deck foreign key,
                  so the row is found by owner + the url the game was using; since we move the
                  row's url forward too, later upgrades keep matching. A deck imported into two
                  live campaigns therefore tracks whichever upgraded last.
                  Skipped for a replacement investigator -- that is a different deck, not an
                  upgrade of this one. -}
                  for_ mDecklist \decklist ->
                    when (sameInvestigator investigatorId decklist) do
                      for_ (attr investigatorDeckUrl investigatorEntity) \oldDeckUrl ->
                        update \d -> do
                          set d
                            $ [ArkhamDeckList =. val decklist]
                            <> [ArkhamDeckUrl =. val decklist.url | isJust decklist.url]
                          where_ $ d.userId ==. val userId
                          where_ $ d.url ==. val (Just oldDeckUrl)

                  pure $ Right g'

  case outcome of
    Left (status, message) -> sendStatusJSON status (JSONError message)
    Right ArkhamGame {..} ->
      publishToRoom gameId
        $ GameUpdate
        $ PublicGame gameId arkhamGameName mempty arkhamGameCurrentData
 where
  deckError :: Text -> Handler a
  deckError = sendStatusJSON Status.status400 . JSONError

  -- True when the decklist upgrades the SAME investigator (an alternate art of them counts)
  -- rather than replacing them with a different one.
  sameInvestigator :: InvestigatorId -> ArkhamDBDecklist -> Bool
  sameInvestigator iid decklist =
    iid
      == decklist.investigator
      || maybe
        False
        ((toCardCode iid `elem`) . (.cardCodes))
        (Map.lookup (toCardCode decklist.investigator) allInvestigatorCards)

  resolveDecklist :: UpgradeDeckPost -> Handler (Maybe ArkhamDBDecklist)
  resolveDecklist postData = case udpDeckList postData of
    Just decklist -> Just <$> validateDecklist decklist
    -- Neither a decklist nor a url: "continue without upgrading".
    Nothing -> for (udpDeckUrl postData) \url ->
      -- getDeckList's http call throws on a non-2xx response rather than returning Left.
      try @_ @SomeException (getDeckList url) >>= \case
        Right (Right decklist) -> validateDecklist decklist
        Right (Left err) -> deckError $ "Could not read the deck at " <> url <> ": " <> T.pack err
        Left _ -> deckError $ "Could not fetch the deck at " <> url

  validateDecklist :: ArkhamDBDecklist -> Handler ArkhamDBDecklist
  validateDecklist decklist = case toDeckErrors decklist of
    [] -> pure decklist
    errs ->
      deckError
        $ "This deck contains cards that are not implemented yet: "
        <> T.intercalate ", " [tshow cCode | UnimplementedCard cCode <- errs]

fromPostData :: UserId -> CreateDeckPost -> ArkhamDeck
fromPostData userId CreateDeckPost {..} = do
  ArkhamDeck
    { arkhamDeckUserId = userId
    , arkhamDeckUrl = deckUrl
    , arkhamDeckInvestigatorName = tshow $ investigator_name deckList
    , arkhamDeckName = deckName
    , arkhamDeckList = deckList
    }

arkhamBuildDecklistUrl :: Text -> Maybe Text
arkhamBuildDecklistUrl url = do
  rest <-
    asum
      $ map
        (`T.stripPrefix` url)
        [ "https://arkham.build/decklist/view/"
        , "https://arkham.build/decklist/"
        , "http://arkham.build/decklist/view/"
        , "http://arkham.build/decklist/"
        ]
  let decklistId = T.takeWhile (/= '?') $ T.takeWhile (/= '/') rest
  guard $ not $ T.null decklistId
  pure $ "https://api.arkham.build/v1/public/arkhamdb/decklist/" <> decklistId

decodeDeckList :: BSL.ByteString -> Either String ArkhamDBDecklist
decodeDeckList bytes = case eitherDecode bytes of
  Right decklist -> Right decklist
  Left _ -> do
    decklists <- eitherDecode bytes
    maybe (Left "No decklist found") Right (listToMaybe decklists)

getDeckList :: MonadIO m => Text -> m (Either String ArkhamDBDecklist)
getDeckList url = liftIO case arkhamBuildDecklistUrl url of
  Just fetchUrl -> do
    request <- parseRequest $ T.unpack fetchUrl
    manager <- newManager tlsManagerSettings
    let request' = request {requestHeaders = ("X-Client-Id", "arkham-horror") : requestHeaders request}
    second (\d -> d {url = Nothing}) . decodeDeckList . responseBody <$> httpLbs request' manager
  Nothing -> second (\d -> d {url = Just url}) . decodeDeckList <$> simpleHttp (T.unpack url)

getApiV1ArkhamDeckR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
getApiV1ArkhamDeckR deckId = do
  userId <- getRequestUserId
  mDeck <- runDB $ selectOne do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.id ==. val deckId
    where_ $ decks.userId ==. val userId
    pure decks
  maybe notFound pure mDeck

deleteApiV1ArkhamDeckR :: ArkhamDeckId -> Handler ()
deleteApiV1ArkhamDeckR deckId = do
  userId <- getRequestUserId
  runDB $ delete do
    decks <- from $ table @ArkhamDeck
    where_ $ decks.id ==. val deckId
    where_ $ decks.userId ==. val userId

newtype JSONError = JSONError {errorMsg :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

postApiV1ArkhamSyncDeckR :: ArkhamDeckId -> Handler (Entity ArkhamDeck)
postApiV1ArkhamSyncDeckR deckId = do
  userId <- getRequestUserId
  deck <- runDB $ get404 deckId
  unless (arkhamDeckUserId deck == userId) do
    sendStatusJSON
      Status.status400
      (JSONError "Deck does not belong to this user")
  edecklist <- maybe (pure $ Left "no deck url") getDeckList (arkhamDeckUrl deck)
  case edecklist of
    Right decklist -> do
      runDB $ update \d -> do
        set d [ArkhamDeckList =. val decklist]
        where_ $ d.id ==. val deckId
      pure $ Entity deckId $ deck {arkhamDeckList = decklist}
    Left _ -> sendStatusJSON Status.status400 (JSONError "Could not sync deck")
