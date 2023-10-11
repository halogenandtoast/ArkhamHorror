{-# LANGUAGE AllowAmbiguousTypes #-}

module Entity.Answer where

import Import.NoFoundation

import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Decklist
import Arkham.Entities
import Arkham.Game
import Arkham.Id
import Arkham.Investigator.Types (InvestigatorAttrs (investigatorPlayerId))
import Arkham.Message
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foundation
import Json
import Safe (fromJustNote)

data Answer
  = Answer QuestionResponse
  | Raw Message
  | PaymentAmountsAnswer PaymentAmountsResponse
  | AmountsAnswer AmountsResponse
  | StandaloneSettingsAnswer [StandaloneSetting]
  | CampaignSettingsAnswer CampaignSettings
  | DeckAnswer {deckId :: ArkhamDeckId}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data QuestionResponse = QuestionResponse
  { qrChoice :: Int
  , qrPlayerId :: Maybe PlayerId
  }
  deriving stock (Show, Generic)

newtype PaymentAmountsResponse = PaymentAmountsResponse
  {parAmounts :: Map InvestigatorId Int}
  deriving stock (Show, Generic)

newtype AmountsResponse = AmountsResponse
  {arAmounts :: Map Text Int}
  deriving stock (Show, Generic)

instance FromJSON QuestionResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "qr"

instance FromJSON PaymentAmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "par"

instance FromJSON AmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ar"

data StandaloneSetting
  = SetKey CampaignLogKey Bool
  | SetRecorded CampaignLogKey SomeRecordableType [SetRecordedEntry]
  | SetOption CampaignOption Bool
  | ChooseNum CampaignLogKey Int
  | NoChooseRecord
  deriving stock (Show)

data SetRecordedEntry
  = SetAsCrossedOut Json.Value
  | SetAsRecorded Json.Value
  | DoNotRecord Json.Value
  deriving stock (Show)

makeStandaloneCampaignLog :: [StandaloneSetting] -> CampaignLog
makeStandaloneCampaignLog = foldl' applySetting mkCampaignLog
 where
  applySetting :: CampaignLog -> StandaloneSetting -> CampaignLog
  applySetting cl NoChooseRecord = cl
  applySetting cl (ChooseNum k n) = setCampaignLogRecordedCount k n cl
  applySetting cl (SetKey k True) = setCampaignLogKey k cl
  applySetting cl (SetKey k False) = deleteCampaignLogKey k cl
  applySetting cl (SetOption k True) = setCampaignLogOption k cl
  applySetting cl (SetOption _ False) = cl
  applySetting cl (SetRecorded k rt vs) = case rt of
    (SomeRecordableType RecordableCardCode) ->
      let entries = mapMaybe (toEntry @CardCode) vs
       in setCampaignLogRecorded k entries cl
    (SomeRecordableType RecordableMemento) ->
      let entries = mapMaybe (toEntry @Memento) vs
       in setCampaignLogRecorded k entries cl
  toEntry :: forall a. Recordable a => SetRecordedEntry -> Maybe SomeRecorded
  toEntry (SetAsRecorded e) = case fromJSON @a e of
    Success a -> Just (recorded a)
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err
  toEntry (SetAsCrossedOut e) = case fromJSON @a e of
    Success a -> Just (crossedOut a)
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err
  toEntry (DoNotRecord _) = Nothing

instance FromJSON StandaloneSetting where
  parseJSON = withObject "StandaloneSetting" $ \o -> do
    t <- o .: "type"
    case t of
      "ChooseRecord" -> do
        mSelected <- o .: "selected"
        case mSelected of
          Nothing -> pure NoChooseRecord
          Just selected -> SetRecorded selected <$> o .: "recordable" <*> ((: []) . SetAsRecorded <$> o .: "key")
      "ToggleKey" -> SetKey <$> o .: "key" <*> o .: "content"
      "ToggleOption" -> SetOption <$> o .: "key" <*> o .: "content"
      "PickKey" -> (`SetKey` True) <$> o .: "content"
      "ToggleCrossedOut" -> do
        k <- o .: "key"
        rt <- o .: "recordable"
        CrossedOutResults v <- o .: "content"
        pure $ SetRecorded k rt v
      "ToggleRecords" -> SetRecorded <$> o .: "key" <*> o .: "recordable" <*> o .: "content"
      "ChooseNum" -> ChooseNum <$> o .: "key" <*> o .: "content"
      _ -> fail $ "No such standalone setting " <> t

instance FromJSON SetRecordedEntry where
  parseJSON = withObject "SetRecordedEntry" $ \o -> do
    k <- o .: "key"
    v <- o .: "content"
    pure $ if v then SetAsRecorded k else DoNotRecord k

newtype CrossedOutResults = CrossedOutResults [SetRecordedEntry]
  deriving stock (Show)

instance FromJSON CrossedOutResults where
  parseJSON jdata = do
    xs <- parseJSON jdata
    let
      toCrossedOutVersion = \case
        DoNotRecord k -> SetAsRecorded k
        SetAsCrossedOut a -> SetAsCrossedOut a
        SetAsRecorded a -> SetAsCrossedOut a
    pure $ CrossedOutResults $ map toCrossedOutVersion xs

data CampaignRecorded = CampaignRecorded
  { recordable :: SomeRecordableType
  , entries :: [CampaignRecordedEntry]
  }
  deriving stock (Show)

data CampaignRecordedEntry
  = CampaignEntryRecorded Json.Value
  | CampaignEntryCrossedOut Json.Value
  deriving stock (Show)

instance FromJSON CampaignRecordedEntry where
  parseJSON = withObject "CampaignRecordedEntry" $ \o -> do
    t :: Text <- o .: "tag"
    case t of
      "CrossedOut" -> CampaignEntryCrossedOut <$> o .: "value"
      "Recorded" -> CampaignEntryRecorded <$> o .: "value"
      _ -> fail $ "Invalid key" <> T.unpack t

data CampaignSettings = CampaignSettings
  { keys :: [CampaignLogKey]
  , counts :: Map CampaignLogKey Int
  , sets :: Map CampaignLogKey CampaignRecorded
  , options :: [CampaignOption]
  }
  deriving stock (Show)

instance FromJSON CampaignSettings where
  parseJSON = withObject "CampaignSettings" $ \o ->
    CampaignSettings
      <$> (o .: "keys")
      <*> (o .: "counts")
      <*> (o .: "sets")
      <*> (o .: "options")

instance FromJSON CampaignRecorded where
  parseJSON = withObject "CampaignRecorded" $ \o ->
    CampaignRecorded
      <$> (o .: "recordable")
      <*> (o .: "entries")

makeCampaignLog :: CampaignSettings -> CampaignLog
makeCampaignLog settings =
  mkCampaignLog
    { campaignLogRecorded = fromList (keys settings)
    , campaignLogRecordedCounts = counts settings
    , campaignLogRecordedSets = fmap toSomeRecorded $ sets settings
    , campaignLogOrderedKeys = keys settings
    , campaignLogOptions = fromList (options settings)
    }
 where
  toSomeRecorded :: CampaignRecorded -> [SomeRecorded]
  toSomeRecorded (CampaignRecorded rt entries) =
    case rt of
      (SomeRecordableType RecordableCardCode) -> map (toEntry @CardCode) entries
      (SomeRecordableType RecordableMemento) -> map (toEntry @Memento) entries
  toEntry :: forall a. Recordable a => CampaignRecordedEntry -> SomeRecorded
  toEntry (CampaignEntryRecorded e) = case fromJSON @a e of
    Success a -> recorded a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err
  toEntry (CampaignEntryCrossedOut e) = case fromJSON @a e of
    Success a -> crossedOut a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err

answerPlayer :: Answer -> Maybe PlayerId
answerPlayer = \case
  Answer response -> qrPlayerId response
  Raw _ -> Nothing
  AmountsAnswer _ -> Nothing
  PaymentAmountsAnswer _ -> Nothing
  StandaloneSettingsAnswer _ -> Nothing
  CampaignSettingsAnswer _ -> Nothing
  DeckAnswer _ -> Nothing

playerInvestigator :: Entities -> PlayerId -> InvestigatorId
playerInvestigator Entities {..} pid = case find ((== pid) . attr investigatorPlayerId) (toList entitiesInvestigators) of
  Just investigator -> toId investigator
  Nothing -> error $ "No investigator for player " <> tshow pid

handleAnswer :: (CanRunDB m, MonadHandler m) => Game -> PlayerId -> Answer -> m [Message]
handleAnswer Game {..} playerId = \case
  DeckAnswer deckId -> do
    deck <- runDB $ get404 deckId
    player <- runDB $ get404 (coerce playerId)
    when (arkhamDeckUserId deck /= arkhamPlayerUserId player) notFound
    let investigatorId = investigator_code $ arkhamDeckList deck
    runDB $ update (traceShowId $ coerce playerId) [ArkhamPlayerInvestigatorId =. coerce investigatorId]
    pure [LoadDecklist playerId (arkhamDeckList deck)]
  StandaloneSettingsAnswer settings' -> do
    let standaloneCampaignLog = makeStandaloneCampaignLog settings'
    pure [SetCampaignLog standaloneCampaignLog]
  CampaignSettingsAnswer settings' -> do
    let campaignLog' = makeCampaignLog settings'
    pure [SetCampaignLog campaignLog']
  AmountsAnswer response -> case Map.lookup playerId gameQuestion of
    Just (ChooseAmounts _ _ _ target) ->
      pure
        [ ResolveAmounts
            (playerInvestigator gameEntities playerId)
            (Map.toList $ arAmounts response)
            target
        ]
    Just (QuestionLabel _ _ (ChooseAmounts _ _ _ target)) ->
      pure
        [ ResolveAmounts
            (playerInvestigator gameEntities playerId)
            (Map.toList $ arAmounts response)
            target
        ]
    _ -> error "Wrong question type"
  PaymentAmountsAnswer response ->
    case Map.lookup playerId gameQuestion of
      Just (ChoosePaymentAmounts _ _ info) -> do
        let
          costMap =
            Map.fromList
              $ map (\(PaymentAmountChoice iid _ _ cost) -> (iid, cost)) info
        pure
          $ concatMap
            ( \(iid, n) ->
                replicate n (Map.findWithDefault Noop iid costMap)
            )
          $ Map.toList (parAmounts response)
      _ -> error "Wrong question type"
  Raw message -> pure [message]
  Answer response -> do
    let
      q =
        fromJustNote
          "Invalid question type"
          (Map.lookup playerId gameQuestion)
    pure $ go id q response
 where
  go
    :: (Question Message -> Question Message)
    -> Question Message
    -> QuestionResponse
    -> [Message]
  go f q response = case q of
    QuestionLabel lbl mCard q' -> go (QuestionLabel lbl mCard) q' response
    Read t qs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ Read t qs]
      Just msg -> [uiToRun msg]
    ChooseOne qs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ ChooseOne qs]
      Just msg -> [uiToRun msg]
    ChooseN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', msgs'') ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask playerId $ f $ ChooseN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask playerId $ f $ ChooseN n msgs'']
    ChooseUpToN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m'@(Done _), _) -> [uiToRun m']
        (Just m', msgs'') ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask playerId $ f $ ChooseUpToN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask playerId $ f $ ChooseUpToN n msgs'']
    ChooseOneAtATime msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', msgs'') ->
          [uiToRun m', Ask playerId $ f $ ChooseOneAtATime msgs'']
        (Nothing, msgs'') ->
          [Ask playerId $ f $ ChooseOneAtATime msgs'']
    ChooseSome msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just (Done _), _) -> []
        (Just m', msgs'') -> case msgs'' of
          [] -> [uiToRun m']
          [Done _] -> [uiToRun m']
          rest -> [uiToRun m', Ask playerId $ f $ ChooseSome rest]
        (Nothing, msgs'') -> [Ask playerId $ f $ ChooseSome msgs'']
    ChooseSome1 doneMsg msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just (Done _), _) -> []
        (Just m', msgs'') -> case msgs'' of
          [] -> [uiToRun m']
          [Done _] -> [uiToRun m']
          rest -> [uiToRun m', Ask playerId $ f $ ChooseSome $ Done doneMsg : rest]
        (Nothing, msgs'') -> [Ask playerId $ f $ ChooseSome $ Done doneMsg : msgs'']
    PickSupplies remaining chosen qs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ PickSupplies remaining chosen qs]
      Just msg -> [uiToRun msg]
    DropDown qs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ DropDown qs]
      Just (_, msg) -> [msg]
    _ -> error "Wrong question type"

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [x | (i, x) <- zip [0 ..] xs, i /= n])
