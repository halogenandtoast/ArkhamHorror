{-# LANGUAGE AllowAmbiguousTypes #-}

module Entity.Answer where

import Import.NoFoundation

import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card.CardCode
import Arkham.Game
import Arkham.Id
import Arkham.Message
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Json
import Safe (fromJustNote)

data Answer
  = Answer QuestionResponse
  | PaymentAmountsAnswer PaymentAmountsResponse
  | AmountsAnswer AmountsResponse
  | StandaloneSettingsAnswer [StandaloneSetting]
  | CampaignSettingsAnswer CampaignSettings
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data QuestionResponse = QuestionResponse
  { qrChoice :: Int
  , qrInvestigatorId :: Maybe InvestigatorId
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
  deriving stock (Show)

data SetRecordedEntry
  = SetAsCrossedOut Json.Value
  | SetAsRecorded Json.Value
  deriving stock (Show)

makeStandaloneCampaignLog :: [StandaloneSetting] -> CampaignLog
makeStandaloneCampaignLog = foldl' applySetting mkCampaignLog
 where
  applySetting :: CampaignLog -> StandaloneSetting -> CampaignLog
  applySetting cl (SetKey k True) = setCampaignLogKey k cl
  applySetting cl (SetKey k False) = deleteCampaignLogKey k cl
  applySetting cl (SetOption k True) = setCampaignLogOption k cl
  applySetting cl (SetOption _ False) = cl
  applySetting cl (SetRecorded k rt vs) =
    case rt of
      (SomeRecordableType RecordableCardCode) ->
        let entries = map (toEntry @CardCode) vs
        in  setCampaignLogRecorded k entries cl
      (SomeRecordableType RecordableMemento) ->
        let entries = map (toEntry @Memento) vs
        in  setCampaignLogRecorded k entries cl
  toEntry :: forall a. Recordable a => SetRecordedEntry -> SomeRecorded
  toEntry (SetAsRecorded e) = case fromJSON @a e of
    Success a -> recorded a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err
  toEntry (SetAsCrossedOut e) = case fromJSON @a e of
    Success a -> crossedOut a
    Error err -> error $ "Failed to parse " <> tshow e <> ": " <> T.pack err

instance FromJSON StandaloneSetting where
  parseJSON = withObject "StandaloneSetting" $ \o -> do
    t <- o .: "type"
    case t of
      "ToggleKey" -> do
        k <- o .: "key"
        v <- o .: "content"
        pure $ SetKey k v
      "ToggleOption" -> do
        k <- o .: "key"
        v <- o .: "content"
        pure $ SetOption k v
      "PickKey" -> do
        k <- o .: "content"
        pure $ SetKey k True
      "ToggleCrossedOut" -> do
        k <- o .: "key"
        rt <- o .: "recordable"
        v <- o .: "content"
        pure $ SetRecorded k rt v
      _ -> fail $ "No such standalone setting" <> t

instance FromJSON SetRecordedEntry where
  parseJSON = withObject "SetRecordedEntry" $ \o -> do
    k <- o .: "key"
    v <- o .: "content"
    pure $ case v of
      True -> SetAsCrossedOut k
      False -> SetAsRecorded k

data CampaignRecorded = CampaignRecorded
  { recordable :: SomeRecordableType
  , entries :: [CampaignRecordedEntry]
  }
  deriving stock (Show)

data CampaignRecordedEntry = CampaignEntryRecorded Json.Value | CampaignEntryCrossedOut Json.Value
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
  parseJSON = withObject "CampaignSettings" $ \o -> do
    keys <- o .: "keys"
    counts <- o .: "counts"
    sets <- o .: "sets"
    options <- o .: "options"
    pure $ CampaignSettings keys counts sets options

instance FromJSON CampaignRecorded where
  parseJSON = withObject "CampaignRecorded" $ \o -> do
    rt <- o .: "recordable"
    entries <- o .: "entries"
    pure $ CampaignRecorded rt entries

makeCampaignLog :: CampaignSettings -> CampaignLog
makeCampaignLog settings =
  mkCampaignLog
    { campaignLogRecorded = Set.fromList (keys settings)
    , campaignLogRecordedCounts = counts settings
    , campaignLogRecordedSets = fmap toSomeRecorded $ sets settings
    , campaignLogOrderedKeys = keys settings
    , campaignLogOptions = Set.fromList (options settings)
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

answerInvestigator :: Answer -> Maybe InvestigatorId
answerInvestigator = \case
  Answer response -> qrInvestigatorId response
  AmountsAnswer _ -> Nothing
  PaymentAmountsAnswer _ -> Nothing
  StandaloneSettingsAnswer _ -> Nothing
  CampaignSettingsAnswer _ -> Nothing

handleAnswer :: Game -> InvestigatorId -> Answer -> [Message]
handleAnswer Game {..} investigatorId = \case
  StandaloneSettingsAnswer settings' ->
    let standaloneCampaignLog = makeStandaloneCampaignLog settings'
    in  [SetCampaignLog standaloneCampaignLog]
  CampaignSettingsAnswer settings' ->
    let campaignLog' = makeCampaignLog settings'
    in  [SetCampaignLog campaignLog']
  AmountsAnswer response -> case Map.lookup investigatorId gameQuestion of
    Just (ChooseAmounts _ _ _ target) ->
      [ ResolveAmounts
          investigatorId
          (Map.toList $ arAmounts response)
          target
      ]
    Just (QuestionLabel _ _ (ChooseAmounts _ _ _ target)) ->
      [ ResolveAmounts
          investigatorId
          (Map.toList $ arAmounts response)
          target
      ]
    _ -> error "Wrong question type"
  PaymentAmountsAnswer response ->
    case Map.lookup investigatorId gameQuestion of
      Just (ChoosePaymentAmounts _ _ info) ->
        let
          costMap =
            Map.fromList $
              map (\(PaymentAmountChoice iid _ _ cost) -> (iid, cost)) info
        in
          concatMap
            ( \(iid, n) ->
                replicate n (Map.findWithDefault Noop iid costMap)
            )
            $ Map.toList (parAmounts response)
      _ -> error "Wrong question type"
  Answer response ->
    let
      q =
        fromJustNote
          "Invalid question type"
          (Map.lookup investigatorId gameQuestion)
    in
      go id q response
 where
  go
    :: (Question Message -> Question Message)
    -> Question Message
    -> QuestionResponse
    -> [Message]
  go f q response = case q of
    QuestionLabel lbl mCard q' -> go (QuestionLabel lbl mCard) q' response
    Read t qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ Read t qs]
      Just msg -> [uiToRun msg]
    ChooseOne qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ ChooseOne qs]
      Just msg -> [uiToRun msg]
    ChooseN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', msgs'') ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask investigatorId $ f $ ChooseN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseN n msgs'']
    ChooseUpToN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m'@(Done _), _) -> [uiToRun m']
        (Just m', msgs'') ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask investigatorId $ f $ ChooseUpToN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseUpToN n msgs'']
    ChooseOneAtATime msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', msgs'') ->
          [uiToRun m', Ask investigatorId $ f $ ChooseOneAtATime msgs'']
        (Nothing, msgs'') ->
          [Ask investigatorId $ f $ ChooseOneAtATime msgs'']
    ChooseSome msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just (Done _), _) -> []
        (Just m', msgs'') -> case msgs'' of
          [] -> [uiToRun m']
          [Done _] -> [uiToRun m']
          rest -> [uiToRun m', Ask investigatorId $ f $ ChooseSome rest]
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseSome msgs'']
    ChooseSome1 doneMsg msgs -> do
      let (mm, msgs') = extract (qrChoice response) msgs
      case (mm, msgs') of
        (Just (Done _), _) -> []
        (Just m', msgs'') -> case msgs'' of
          [] -> [uiToRun m']
          [Done _] -> [uiToRun m']
          rest -> [uiToRun m', Ask investigatorId $ f $ ChooseSome $ Done doneMsg : rest]
        (Nothing, msgs'') -> [Ask investigatorId $ f $ ChooseSome $ Done doneMsg : msgs'']
    PickSupplies remaining chosen qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ PickSupplies remaining chosen qs]
      Just msg -> [uiToRun msg]
    DropDown qs -> case qs !!? qrChoice response of
      Nothing -> [Ask investigatorId $ f $ DropDown qs]
      Just (_, msg) -> [msg]
    _ -> error "Wrong question type"

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [x | (i, x) <- zip [0 ..] xs, i /= n])
