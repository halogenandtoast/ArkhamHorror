{-# LANGUAGE AllowAmbiguousTypes #-}

module Entity.Answer where

import Import.NoFoundation hiding (get)

import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Partner
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Cost
import Arkham.Decklist
import Arkham.Entities
import Arkham.Game
import Arkham.Id
import Arkham.Investigator.Types (InvestigatorAttrs (investigatorPlayerId))
import Arkham.Message
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID (UUID)
import Foundation
import Json

data Answer
  = Answer QuestionResponse
  | Raw Message
  | PaymentAmountsAnswer PaymentAmountsResponse
  | AmountsAnswer AmountsResponse
  | StandaloneSettingsAnswer [StandaloneSetting]
  | CampaignSettingsAnswer CampaignSettings
  | DeckAnswer {deckId :: ArkhamDeckId, playerId :: PlayerId}
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data QuestionResponse = QuestionResponse
  { qrChoice :: Int
  , qrPlayerId :: Maybe PlayerId
  }
  deriving stock (Show, Generic)

newtype PaymentAmountsResponse = PaymentAmountsResponse
  {parAmounts :: Map UUID Int}
  deriving stock (Show, Generic)

newtype AmountsResponse = AmountsResponse
  {arAmounts :: Map UUID Int}
  deriving stock (Show, Generic)

data PartnerDetailsResponse = PartnerDetailsResponse
  { damage :: Int
  , horror :: Int
  , status :: PartnerStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

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
  | StandaloneSetPartnerStatus Int Int PartnerStatus Bool CardCode
  | SettingsGroup [StandaloneSetting]
  deriving stock Show

data SetRecordedEntry
  = SetAsCrossedOut Json.Value
  | SetAsRecorded Json.Value
  | DoNotRecord Json.Value
  deriving stock Show

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
  applySetting cl (StandaloneSetPartnerStatus dmg hrr status crash cCode) =
    if crash
      then
        setCampaignLogRecorded (EdgeOfTheEarthKey WasKilledInThePlaneCrash) [recorded cCode]
          $ setCampaignLogPartnerStatus dmg hrr status cCode cl
      else setCampaignLogPartnerStatus dmg hrr status cCode cl
  applySetting c1 (SettingsGroup xs) = foldl' applySetting c1 xs
  applySetting cl (SetRecorded k rt vs) = case rt of
    (SomeRecordableType RecordableCardCode) ->
      let entries = mapMaybe (toEntry @CardCode) vs
       in setCampaignLogRecorded k entries cl
    (SomeRecordableType RecordableMemento) ->
      let entries = mapMaybe (toEntry @Memento) vs
       in setCampaignLogRecorded k entries cl
    (SomeRecordableType RecordableMemory) ->
      let entries = mapMaybe (toEntry @Memory) vs
       in setCampaignLogRecorded k entries cl
    (SomeRecordableType RecordableGeneric) ->
      let entries = mapMaybe (toEntry @Value) vs
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
      "Group" -> SettingsGroup <$> o .: "content"
      "SetPartnerKilled" -> StandaloneSetPartnerStatus 0 0 Eliminated True <$> o .: "content"
      "SetPartnerDetails" -> do
        details :: PartnerDetailsResponse <- o .: "content"
        cCode <- o .: "value"
        pure
          $ StandaloneSetPartnerStatus details.damage details.horror details.status False
          $ if details.status == Resolute then toResolute cCode else cCode
      _ -> fail $ "No such standalone setting " <> t

instance FromJSON SetRecordedEntry where
  parseJSON = withObject "SetRecordedEntry" $ \o -> do
    k <- o .: "key"
    v <- o .: "content"
    pure $ if v then SetAsRecorded k else DoNotRecord k

newtype CrossedOutResults = CrossedOutResults [SetRecordedEntry]
  deriving stock Show

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
  deriving stock Show

data CampaignRecordedEntry
  = CampaignEntryRecorded Json.Value
  | CampaignEntryCrossedOut Json.Value
  deriving stock Show

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
  deriving stock Show

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
      (SomeRecordableType RecordableMemory) -> map (toEntry @Memory) entries
      (SomeRecordableType RecordableGeneric) -> map (toEntry @Value) entries
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
  DeckAnswer _ pid -> Just pid

playerInvestigator :: Entities -> PlayerId -> InvestigatorId
playerInvestigator Entities {..} pid = case find ((== pid) . attr investigatorPlayerId) (toList entitiesInvestigators) of
  Just investigator -> toId investigator
  Nothing -> error $ "No investigator for player " <> tshow pid

data Reply = Handled [Message] | Unhandled Text

handled :: Applicative m => [Message] -> m Reply
handled = pure . Handled

unhandled :: Applicative m => Text -> m Reply
unhandled = pure . Unhandled

handleAnswer :: (CanRunDB m, MonadHandler m) => Game -> PlayerId -> Answer -> m Reply
handleAnswer Game {..} playerId = \case
  DeckAnswer deckId _ -> do
    deck <- runDB $ get404 deckId
    let investigatorId = investigator_code $ arkhamDeckList deck
    runDB $ update (coerce playerId) [ArkhamPlayerInvestigatorId =. coerce investigatorId]
    let question' = Map.delete playerId gameQuestion
    handled $ LoadDecklist playerId (arkhamDeckList deck) : [AskMap question' | not (Map.null question')]
  StandaloneSettingsAnswer settings' -> do
    let standaloneCampaignLog = makeStandaloneCampaignLog settings'
    handled [SetCampaignLog standaloneCampaignLog]
  CampaignSettingsAnswer settings' -> do
    let campaignLog' = makeCampaignLog settings'
    handled [SetCampaignLog campaignLog']
  AmountsAnswer response -> case Map.lookup playerId gameQuestion of
    Just (ChooseAmounts _ _ choices target) -> do
      let nameMap = Map.fromList $ map (\(AmountChoice cId lbl _ _) -> (cId, lbl)) choices
      let toNamedUUID uuid = NamedUUID (Map.findWithDefault (error "Missing key") uuid nameMap) uuid
      let question' = Map.delete playerId gameQuestion
      let amounts = map (first toNamedUUID) $ Map.toList $ arAmounts response
      handled
        $ ResolveAmounts (playerInvestigator gameEntities playerId) amounts target
        : [AskMap question' | not (Map.null question')]
    Just (QuestionLabel _ _ (ChooseAmounts _ _ choices target)) -> do
      let nameMap = Map.fromList $ map (\(AmountChoice cId lbl _ _) -> (cId, lbl)) choices
      let toNamedUUID uuid = NamedUUID (Map.findWithDefault (error "Missing key") uuid nameMap) uuid
      let question' = Map.delete playerId gameQuestion
      let amounts = map (first toNamedUUID) $ Map.toList $ arAmounts response
      handled
        $ ResolveAmounts (playerInvestigator gameEntities playerId) amounts target
        : [AskMap question' | not (Map.null question')]
    _ -> unhandled "Wrong question type"
  PaymentAmountsAnswer response ->
    case Map.lookup playerId gameQuestion of
      Just (ChoosePaymentAmounts _ _ info) -> do
        let costMap = Map.fromList $ map (\(PaymentAmountChoice cId _ _ _ _ cost) -> (cId, cost)) info
        let
          combinePaymentAmounts n = \case
            PayCost acId iid skip (UseCost aMatcher uType m) -> [PayCost acId iid skip (UseCost aMatcher uType (n * m))]
            PayCost acId iid skip (ResourceCost _) | n == 0 -> [PayCost acId iid skip (ResourceCost 0)]
            payMsg -> replicate n payMsg
        let handleCost (cId, n) = combinePaymentAmounts n $ Map.findWithDefault Noop cId costMap
        handled $ concatMap handleCost $ Map.toList (parAmounts response)
      _ -> unhandled "Wrong question type"
  Raw message -> do
    let isPlayerWindowChoose = \case
          PlayerWindowChooseOne _ -> True
          _ -> False
    if not (Map.null gameQuestion) && not (any isPlayerWindowChoose $ toList gameQuestion)
      then case message of
        PassSkillTest -> handled [message]
        FailSkillTest -> handled [message]
        ForceChaosTokenDraw _ -> handled [message]
        _ -> handled [message, AskMap gameQuestion]
      else handled [message]
  Answer response -> do
    maybe (unhandled "Player not being asked") (\q -> handled $ go id q response)
      $ Map.lookup playerId gameQuestion
 where
  go
    :: (Question Message -> Question Message)
    -> Question Message
    -> QuestionResponse
    -> [Message]
  go f q response = case q of
    QuestionLabel lbl mCard q' -> go (QuestionLabel lbl mCard) q' response
    Read t (BasicReadChoices qs) mcs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ Read t (BasicReadChoices qs) mcs]
      Just msg -> [uiToRun msg]
    Read t (BasicReadChoicesN n qs) mcs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', _) ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask playerId $ f $ Read t (BasicReadChoicesN (n - 1) msgs') mcs]
        (Nothing, msgs'') -> [Ask playerId $ f $ Read t (BasicReadChoicesN n msgs'') mcs]
    Read t (BasicReadChoicesUpToN n qs) mcs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m'@(Done _), _) -> [uiToRun m']
        (Just m', [Done _]) -> [uiToRun m']
        (Just m', msgs'') ->
          if n - 1 == 0
            then [uiToRun m']
            else [uiToRun m', Ask playerId $ f $ Read t (BasicReadChoicesUpToN (n - 1) msgs'') mcs]
        (Nothing, msgs'') -> [Ask playerId $ f $ Read t (BasicReadChoicesUpToN n msgs'') mcs]
    Read t (LeadInvestigatorMustDecide qs) mcs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ Read t (LeadInvestigatorMustDecide qs) mcs]
      Just msg -> [uiToRun msg]
    ChooseOne qs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ ChooseOne qs]
      Just msg -> [uiToRun msg]
    PlayerWindowChooseOne qs -> case qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ PlayerWindowChooseOne qs]
      Just msg -> [uiToRun msg]
    ChooseOneFromEach qs -> case concat qs !!? qrChoice response of
      Nothing -> [Ask playerId $ f $ ChooseOneFromEach qs]
      Just msg ->
        let
          removeSublistAtIndex :: Int -> [[a]] -> [[a]]
          removeSublistAtIndex idx xss = removeSublist idx xss 0
          removeSublist _ [] _ = []
          removeSublist n (ys : yss) currentIdx
            | n < currentIdx + length ys = yss
            | otherwise = ys : removeSublist n yss (currentIdx + length ys)
          remaining = removeSublistAtIndex (qrChoice response) qs
         in
          uiToRun msg : [Ask playerId $ f $ ChooseOneFromEach remaining | not (null remaining)]
    ChooseN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m', msgs''@(m1 : mrest)) ->
          if n - 1 == 0
            then [uiToRun m']
            else
              -- it is possible that every choice in qs is the same, in which case we can just run them all
              if all (== m1) mrest
                then uiToRun m' : map uiToRun (take (n - 1) msgs'')
                else [uiToRun m', Ask playerId $ f $ ChooseN (n - 1) msgs'']
        (Nothing, msgs'') -> [Ask playerId $ f $ ChooseN n msgs'']
    ChooseUpToN n qs -> do
      let (mm, msgs') = extract (qrChoice response) qs
      case (mm, msgs') of
        (Just m', []) -> [uiToRun m']
        (Just m'@(Done _), _) -> [uiToRun m']
        (Just m', [Done _]) -> [uiToRun m']
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
    ChooseOneAtATimeWithAuto k msgs -> do
      if qrChoice response == 0
        then map uiToRun msgs
        else do
          let (mm, msgs') = extract (qrChoice response - 1) msgs
          case (mm, msgs') of
            (Just m', []) -> [uiToRun m']
            (Just m', msgs'') ->
              if length msgs'' > 1
                then [uiToRun m', Ask playerId $ f $ ChooseOneAtATimeWithAuto k msgs'']
                else [uiToRun m', Ask playerId $ f $ ChooseOneAtATime msgs'']
            (Nothing, msgs'') ->
              [Ask playerId $ f $ ChooseOneAtATimeWithAuto k msgs'']
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
extract n xs = let a = xs !!? n in (a, [x | (i, x) <- zip [0 ..] xs, i /= n])
