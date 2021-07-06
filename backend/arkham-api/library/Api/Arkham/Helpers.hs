{-# LANGUAGE TupleSections #-}
module Api.Arkham.Helpers where

import Import hiding (appLogger)

import Arkham.PlayerCard
import Arkham.Types.Card
import Arkham.Types.Classes.HasQueue
import Arkham.Types.Game
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Name
import Control.Lens
import Control.Monad.Fail
import Control.Monad.Random (MonadRandom(..), StdGen, mkStdGen)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Json

data ApiResponse = GameUpdate (Entity ArkhamGame) | GameMessage Text
  deriving stock Generic
  deriving anyclass ToJSON

newtype GameAppT a = GameAppT { unGameAppT :: ReaderT GameApp IO a }
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom)

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: IORef [Message]
  , appGen :: IORef StdGen
  , appLogger :: Message -> IO ()
  }

newApp :: MonadIO m => Game -> (Message -> IO ()) -> [Message] -> m GameApp
newApp g logger msgs = do
  gameRef <- newIORef g
  queueRef <- newIORef msgs
  genRef <- newIORef (mkStdGen (gameSeed g))
  pure $ GameApp gameRef queueRef genRef logger

instance HasStdGen GameApp where
  genL = lens appGen $ \m x -> m { appGen = x }

instance HasGameRef GameApp where
  gameRefL = lens appGame $ \m x -> m { appGame = x }

instance HasQueue GameApp where
  messageQueue = lens appQueue $ \m x -> m { appQueue = x }

instance HasMessageLogger GameApp where
  messageLoggerL = lens appLogger $ \m x -> m { appLogger = x }

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

noLogger :: Applicative m => Message -> m ()
noLogger = const (pure ())

getChannel :: ArkhamGameId -> Handler (TChan BSL.ByteString)
getChannel gameId = do
  gameChannelsRef <- appGameChannels <$> getYesod
  gameChannels <- readIORef gameChannelsRef
  case Map.lookup gameId gameChannels of
    Just chan -> pure chan
    Nothing -> do
      chan <- atomically newBroadcastTChan
      atomicModifyIORef' gameChannelsRef
        $ \gameChannels' -> (Map.insert gameId chan gameChannels', ())
      pure chan

newtype ArkhamDBDecklistMeta = ArkhamDBDecklistMeta { alternate_front :: InvestigatorId }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

loadDecklistCards :: ArkhamDBDecklist -> IO [PlayerCard]
loadDecklistCards decklist =
  flip HashMap.foldMapWithKey (slots decklist) $ \cardCode count' ->
    if cardCode /= "01000"
      then replicateM count' (genPlayerCard (lookupPlayerCardDef cardCode))
      else pure []

loadDecklist :: ArkhamDeck -> IO (InvestigatorId, [PlayerCard])
loadDecklist arkhamDeck = (investigatorId, ) <$> loadDecklistCards decklist
 where
  decklist = arkhamDeckList arkhamDeck
  investigatorId = case meta decklist of
    Nothing -> investigator_code decklist
    Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
      Nothing -> investigator_code decklist
      Just ArkhamDBDecklistMeta {..} -> alternate_front

displayName :: Name -> Text
displayName (Name title Nothing) = title
displayName (Name title (Just subtitle)) = title <> ": " <> subtitle

displayCardType :: CardType -> Text
displayCardType = \case
  AssetType -> "asset"
  EventType -> "event"
  SkillType -> "skill"
  PlayerTreacheryType -> "treachery"
  PlayerEnemyType -> "enemy"
  TreacheryType -> "treachery"
  EnemyType -> "enemy"
  LocationType -> "location"
  EncounterAssetType -> "asset"

toHumanReadable :: Message -> Maybe Text
toHumanReadable = \case
  BeginEnemy -> Just "Begin enemy phase"
  BeginInvestigation -> Just "Begin investigation phase"
  BeginMythos -> Just "Begin mythos phase"
  BeginUpkeep -> Just "Begin upkeep phase"
  StartScenario name _ -> Just $ "Begin scenario " <> displayName name
  PlayedCard iid cardId name cardCode ->
    Just $ investigator iid <> " played " <> card name cardCode cardId
  InvestigatorMulligan iid ->
    Just $ investigator iid <> " deciding on mulligan"
  FinishedWithMulligan iid -> Just $ investigator iid <> " done with mulligan"
  TakeStartingResources iid ->
    Just $ investigator iid <> " took starting resources"
  ShuffleDiscardBackIn iid ->
    Just $ investigator iid <> " shuffles discarded cards back into deck"
  PlacedLocation name cardCode cardId ->
    Just $ card name cardCode cardId <> " was placed"
  BeginTurn iid -> Just $ "Begin " <> investigator iid <> "'s turn"
  After _ -> Nothing
  FlavorText{} -> Nothing
  CheckWindow{} -> Nothing
  EndCheckWindow{} -> Nothing
  ChoosePlayerOrder{} -> Nothing
  Continue{} -> Nothing
  NextCampaignStep{} -> Nothing
  CampaignStep{} -> Nothing
  ResetGame -> Nothing
  LoadDeck{} -> Nothing
  Run{} -> Nothing
  ChooseLeadInvestigator{} -> Nothing
  SetupInvestigators{} -> Nothing
  PutCardIntoPlay{} -> Nothing
  InvestigatorPlayAsset{} -> Nothing
  SetTokensForScenario{} -> Nothing
  SetTokens{} -> Nothing
  Setup{} -> Nothing
  EndSetup{} -> Nothing
  SetEncounterDeck{} -> Nothing
  AddAgenda{} -> Nothing
  AddAct{} -> Nothing
  PlaceLocation{} -> Nothing
  AddConnection{} -> Nothing
  RevealLocation{} -> Nothing
  RunBag{} -> Nothing
  NextChaosBagStep{} -> Nothing
  TriggerSkillTest{} -> Nothing
  RequestTokens{} -> Nothing
  FocusTokens{} -> Nothing
  UnfocusTokens{} -> Nothing
  When{} -> Nothing
  RevealSkillTestTokens{} -> Nothing
  RevealToken{} -> Nothing
  Will{} -> Nothing
  AddedConnection{} -> Nothing
  SetLocationLabel{} -> Nothing
  CreateStoryAssetAt{} -> Nothing
  ShuffleIntoEncounterDeck{} -> Nothing
  CreateEnemyAt{} -> Nothing
  msg -> Just $ tshow msg
 where
  investigator iid = "{investigator:" <> tshow iid <> "}"
  card name cardCode cardId =
    "{card:\""
      <> displayName name
      <> "\":"
      <> tshow cardCode
      <> ":\""
      <> tshow cardId
      <> "\"}"
