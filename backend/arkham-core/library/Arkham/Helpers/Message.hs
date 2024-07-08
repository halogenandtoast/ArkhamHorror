module Arkham.Helpers.Message (module Arkham.Helpers.Message, module X) where

import Arkham.Prelude

import Arkham.Classes.HasQueue as X hiding (push, pushAll)
import Arkham.Helpers.Message.Discard as X
import Arkham.Message as X

import Arkham.Capability
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue qualified as Queue
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Deck
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Enemy.Creation
import Arkham.Exception
import Arkham.Helpers.Investigator ()
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Label (mkLabel)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Resolution
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Window (Window (..), WindowType, defaultWindows, mkAfter, mkWindow)
import Arkham.Window qualified as Window

drawCards :: Sourceable source => InvestigatorId -> source -> Int -> Message
drawCards i source n = DrawCards i $ newCardDraw source i n

drawEncounterCard :: Sourceable source => InvestigatorId -> source -> Message
drawEncounterCard i source = drawEncounterCards i source 1

drawEncounterCards :: Sourceable source => InvestigatorId -> source -> Int -> Message
drawEncounterCards i source n = DrawCards i $ newCardDraw source Deck.EncounterDeck n

drawCardsIfCan
  :: (MonadRandom m, Sourceable source, HasGame m)
  => InvestigatorId
  -> source
  -> Int
  -> m (Maybe Message)
drawCardsIfCan i source n = do
  canDraw <- can.draw.cards (sourceToFromSource source) i
  pure $ guard canDraw $> drawCards i source n

sourceToFromSource :: Sourceable source => source -> FromSource
sourceToFromSource (toSource -> source) = case source of
  AbilitySource s _ -> sourceToFromSource s
  InvestigatorSource _ -> FromPlayerCardEffect
  AssetSource _ -> FromPlayerCardEffect
  EventSource _ -> FromPlayerCardEffect
  SkillSource _ -> FromPlayerCardEffect
  _ -> FromOtherSource

drawCardsAction :: Sourceable source => InvestigatorId -> source -> Int -> Message
drawCardsAction i source n = DrawCards i $ asDrawAction $ newCardDraw source i n

resolveWithWindow :: HasGame m => Message -> WindowType -> m [Message]
resolveWithWindow msg window' = do
  whenWindow <- checkWindows [mkWindow Timing.When window']
  atIfWindow <- checkWindows [mkWindow Timing.AtIf window']
  afterWindow <- checkWindows [mkWindow Timing.After window']
  pure [When msg, whenWindow, atIfWindow, msg, After msg, afterWindow]

dealAdditionalDamage :: HasQueue Message m => InvestigatorId -> Int -> [Message] -> m ()
dealAdditionalDamage iid amount additionalMessages = do
  mMsg <- findFromQueue $ \case
    InvestigatorDamage iid' _ n _ | iid' == iid -> n > 0
    InvestigatorDoAssignDamage iid' _ _ _ n _ [] [] | iid' == iid -> n > 0
    _ -> False
  case mMsg of
    Just damageMsg -> do
      let
        newMsg = case damageMsg of
          InvestigatorDamage _ source' n horror ->
            InvestigatorDamage iid source' (n + amount) horror
          InvestigatorDoAssignDamage _ source' strategy matcher n horror [] [] ->
            InvestigatorDoAssignDamage
              iid
              source'
              strategy
              matcher
              (n + amount)
              horror
              []
              []
          _ -> error "impossible"
      replaceMessage damageMsg $ newMsg : additionalMessages
    Nothing -> throwIO $ InvalidState "No damage occured"

dealAdditionalHorror :: HasQueue Message m => InvestigatorId -> Int -> [Message] -> m ()
dealAdditionalHorror iid amount additionalMessages = do
  mMsg <- findFromQueue $ \case
    InvestigatorDamage iid' _ _ n | iid' == iid -> n > 0
    InvestigatorDoAssignDamage iid' _ _ _ _ n [] [] | iid' == iid -> n > 0
    _ -> False
  case mMsg of
    Just horrorMsg -> do
      let
        newMsg = case horrorMsg of
          InvestigatorDamage _ source' damage n ->
            InvestigatorDamage iid source' damage (n + amount)
          InvestigatorDoAssignDamage _ source' strategy matcher damage n [] [] ->
            InvestigatorDoAssignDamage
              iid
              source'
              strategy
              matcher
              damage
              (n + amount)
              []
              []
          _ -> error "impossible"
      replaceMessage horrorMsg $ newMsg : additionalMessages
    Nothing -> throwIO $ InvalidState "No horror occured"

cancelHorror
  :: (Sourceable source, HasQueue Message m, HasGame m)
  => InvestigatorId
  -> source
  -> Int
  -> [Message]
  -> m ()
cancelHorror iid (toSource -> source) amount additionalMessages = do
  mMsg <- findFromQueue $ \case
    InvestigatorDamage iid' _ _ n | iid' == iid -> n > 0
    InvestigatorDoAssignDamage iid' _ _ _ _ n [] [] | iid' == iid -> n > 0
    _ -> False
  case mMsg of
    Just horrorMsg -> do
      let
        mNewMsg = case horrorMsg of
          InvestigatorDamage _ source' damage n ->
            guard (n - amount > 0) $> InvestigatorDamage iid source' damage (n - amount)
          InvestigatorDoAssignDamage _ source' strategy matcher damage n [] [] ->
            guard (n - amount > 0)
              $> InvestigatorDoAssignDamage
                iid
                source'
                strategy
                matcher
                damage
                (n - amount)
                []
                []
          _ -> error "impossible"

      ignoreWindow <- checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect source)]
      push ignoreWindow
      replaceMessage horrorMsg $ maybeToList mNewMsg <> additionalMessages
    Nothing -> throwIO $ InvalidState "No horror occured"

createEnemy
  :: (MonadRandom m, IsCard card, IsEnemyCreationMethod creationMethod)
  => card
  -> creationMethod
  -> m (EnemyCreation Message)
createEnemy (toCard -> card) (toEnemyCreationMethod -> cMethod) = do
  enemyId <- getRandom
  pure
    $ MkEnemyCreation
      { enemyCreationCard = card
      , enemyCreationEnemyId = enemyId
      , enemyCreationMethod = cMethod
      , enemyCreationAfter = []
      , enemyCreationExhausted = False
      , enemyCreationTarget = Nothing
      , enemyCreationInvestigator = Nothing
      }

createEnemyWithPlacement :: MonadRandom m => Card -> Placement -> m (EnemyId, Message)
createEnemyWithPlacement c placement = do
  creation <- createEnemy c placement
  pure (enemyCreationEnemyId creation, CreateEnemy creation)

createEnemyWithPlacement_ :: MonadRandom m => Card -> Placement -> m Message
createEnemyWithPlacement_ c placement = snd <$> createEnemyWithPlacement c placement

createEnemyAt :: MonadRandom m => Card -> LocationId -> Maybe Target -> m (EnemyId, Message)
createEnemyAt c lid mTarget = do
  creation <- createEnemy c lid
  pure (enemyCreationEnemyId creation, CreateEnemy $ creation {enemyCreationTarget = mTarget})

createEnemyAt_ :: MonadRandom m => Card -> LocationId -> Maybe Target -> m Message
createEnemyAt_ c lid mTarget = snd <$> createEnemyAt c lid mTarget

createEnemyAtLocationMatching :: MonadRandom m => Card -> LocationMatcher -> m (EnemyId, Message)
createEnemyAtLocationMatching c matcher = do
  creation <- createEnemy c matcher
  pure (enemyCreationEnemyId creation, CreateEnemy creation)

createEnemyAtLocationMatching_ :: MonadRandom m => Card -> LocationMatcher -> m Message
createEnemyAtLocationMatching_ c matcher = snd <$> createEnemyAtLocationMatching c matcher

createEnemyEngagedWithPrey :: MonadRandom m => Card -> m (EnemyId, Message)
createEnemyEngagedWithPrey c = do
  creation <- createEnemy c SpawnEngagedWithPrey
  pure (enemyCreationEnemyId creation, CreateEnemy creation)

createEnemyEngagedWithPrey_ :: MonadRandom m => Card -> m Message
createEnemyEngagedWithPrey_ = fmap snd . createEnemyEngagedWithPrey

placeLocation :: MonadRandom m => Card -> m (LocationId, Message)
placeLocation c = do
  locationId <- getRandom
  pure (locationId, PlaceLocation locationId c)

placeLocation_ :: MonadRandom m => Card -> m Message
placeLocation_ = fmap snd . placeLocation

placeSetAsideLocation :: (MonadRandom m, HasGame m) => CardDef -> m (LocationId, Message)
placeSetAsideLocation = placeLocation <=< getSetAsideCard

placeSetAsideLocation_ :: (MonadRandom m, HasGame m) => CardDef -> m Message
placeSetAsideLocation_ = placeLocation_ <=< getSetAsideCard

placeSetAsideLocations :: (MonadRandom m, HasGame m) => [CardDef] -> m [Message]
placeSetAsideLocations = traverse placeSetAsideLocation_

placeLocationCard :: (CardGen m, HasGame m) => CardDef -> m (LocationId, Message)
placeLocationCard = placeLocation <=< genCard

placeLocationCard_ :: (HasGame m, CardGen m) => CardDef -> m Message
placeLocationCard_ = placeLocation_ <=< genCard

placeLocationCards_ :: (CardGen m, HasGame m) => [CardDef] -> m [Message]
placeLocationCards_ = traverse placeLocationCard_

scenarioResolution :: Int -> Message
scenarioResolution = ScenarioResolution . Resolution

pattern R1 :: Message
pattern R1 = ScenarioResolution (Resolution 1)

pattern R2 :: Message
pattern R2 = ScenarioResolution (Resolution 2)

pattern R3 :: Message
pattern R3 = ScenarioResolution (Resolution 3)

pattern R4 :: Message
pattern R4 = ScenarioResolution (Resolution 4)

pattern R5 :: Message
pattern R5 = ScenarioResolution (Resolution 5)

pattern R6 :: Message
pattern R6 = ScenarioResolution (Resolution 6)

pattern R7 :: Message
pattern R7 = ScenarioResolution (Resolution 7)

pattern R8 :: Message
pattern R8 = ScenarioResolution (Resolution 8)

gainSurge :: (Sourceable a, Targetable a) => a -> Message
gainSurge a = GainSurge (toSource a) (toTarget a)

toDiscard :: (Sourceable source, Targetable target) => source -> target -> Message
toDiscard source target = Discard Nothing (toSource source) (toTarget target)

toDiscardBy
  :: (Sourceable source, Targetable target) => InvestigatorId -> source -> target -> Message
toDiscardBy iid source target = Discard (Just iid) (toSource source) (toTarget target)

pushAllM :: (IsMessage msg, HasQueue Message m) => m [msg] -> m ()
pushAllM mmsgs = do
  msgs <- mmsgs
  Queue.pushAll $ map toMessage msgs

pushAll :: (IsMessage msg, HasQueue Message m) => [msg] -> m ()
pushAll = pushAllM . pure

push :: (IsMessage msg, HasQueue Message m) => msg -> m ()
push = Queue.push . toMessage

pushM :: (HasQueue Message m, IsMessage msg) => m msg -> m ()
pushM mmsg = mmsg >>= push

pushWhenM :: (HasQueue Message m, IsMessage msg) => m Bool -> msg -> m ()
pushWhenM condM = whenM condM . pushAll . pure

pushWhen :: (HasQueue Message m, IsMessage msg) => Bool -> msg -> m ()
pushWhen cond = when cond . push

pushIfAny
  :: (HasQueue Message m, MonoFoldable (t a), IsMessage msg) => t a -> msg -> m ()
pushIfAny collection = when (notNull collection) . push

removeMessageType :: HasQueue Message m => MessageType -> m ()
removeMessageType msgType = withQueue_ $ \queue ->
  let
    (before, after) = break ((== Just msgType) . messageType) queue
    remaining = drop 1 after
   in
    before <> remaining

addToHand :: IsCard a => InvestigatorId -> a -> Message
addToHand i (toCard -> c) = AddToHand i [c]

shuffleIntoDeck :: (IsDeck deck, Targetable target) => deck -> target -> Message
shuffleIntoDeck (toDeck -> deck) (toTarget -> target) = ShuffleIntoDeck deck target

findEncounterCard
  :: (Targetable target, IsCardMatcher cardMatcher)
  => InvestigatorId
  -> target
  -> [ScenarioZone]
  -> cardMatcher
  -> Message
findEncounterCard iid (toTarget -> target) zones (toCardMatcher -> cardMatcher) =
  FindEncounterCard iid target zones cardMatcher

placeLabeledLocationCards_ :: (HasGame m, CardGen m) => Text -> [CardDef] -> m [Message]
placeLabeledLocationCards_ lbl cards = do
  startIndex <- getStartIndex 1
  concatForM (withIndexN startIndex cards) $ \(idx, card) -> do
    (location, placement) <- placeLocationCard card
    pure [placement, SetLocationLabel location (lbl <> tshow idx)]
 where
  getStartIndex n = do
    alreadyTaken <- selectAny $ LocationWithLabel (mkLabel $ lbl <> tshow n)
    if alreadyTaken then getStartIndex (n + 1) else pure n

placeLabeledLocationCards
  :: (HasGame m, CardGen m) => Text -> [CardDef] -> m ([LocationId], [Message])
placeLabeledLocationCards lbl cards = fmap fold
  . concatForM (withIndex1 cards)
  $ \(idx, card) -> do
    (location, placement) <- placeLocationCard card
    pure [([location], [placement, SetLocationLabel location (lbl <> tshow idx)])]

placeLabeledLocations_ :: (HasGame m, CardGen m) => Text -> [Card] -> m [Message]
placeLabeledLocations_ lbl cards = do
  startIndex <- getStartIndex 1
  concatForM (withIndexN startIndex cards) $ \(idx, card) -> do
    (location, placement) <- placeLocation card
    pure [placement, SetLocationLabel location (lbl <> tshow idx)]
 where
  getStartIndex n = do
    alreadyTaken <- selectAny $ LocationWithLabel (mkLabel $ lbl <> tshow n)
    if alreadyTaken then getStartIndex (n + 1) else pure n

placeLabeledLocations
  :: (HasGame m, CardGen m) => Text -> [Card] -> m ([LocationId], [Message])
placeLabeledLocations lbl cards = fmap fold
  . concatForM (withIndex1 cards)
  $ \(idx, card) -> do
    (location, placement) <- placeLocation card
    pure [([location], [placement, SetLocationLabel location (lbl <> tshow idx)])]

placeLabeledLocationsFrom
  :: (HasGame m, CardGen m) => Text -> Int -> [Card] -> m ([LocationId], [Message])
placeLabeledLocationsFrom lbl n cards = fmap fold
  . concatForM (withIndex cards)
  $ \(idx, card) -> do
    (location, placement) <- placeLocation card
    pure [([location], [placement, SetLocationLabel location (lbl <> tshow (n + idx))])]

putCardIntoPlay :: IsCard card => InvestigatorId -> card -> Message
putCardIntoPlay iid (toCard -> card) = PutCardIntoPlay iid card Nothing NoPayment (defaultWindows iid)

placeLabeledLocation :: (MonadRandom m, HasGame m) => Text -> Card -> m (LocationId, Message)
placeLabeledLocation lbl card = do
  idx <- getStartIndex (1 :: Int)
  (location, placement) <- placeLocation card
  pure (location, Run [placement, SetLocationLabel location (lbl <> tshow idx)])
 where
  getStartIndex n = do
    alreadyTaken <- selectAny $ LocationWithLabel (mkLabel $ lbl <> tshow n)
    if alreadyTaken then getStartIndex (n + 1) else pure n

assignDamageLabel :: Sourceable source => InvestigatorId -> source -> Int -> UI Message
assignDamageLabel iid source damage = DamageLabel iid [assignDamage iid source damage]

assignHorrorLabel :: Sourceable source => InvestigatorId -> source -> Int -> UI Message
assignHorrorLabel iid source horror = HorrorLabel iid [assignHorror iid source horror]

assignDamage :: Sourceable source => InvestigatorId -> source -> Int -> Message
assignDamage iid (toSource -> source) damage = InvestigatorAssignDamage iid source DamageAny damage 0

assignHorror :: Sourceable source => InvestigatorId -> source -> Int -> Message
assignHorror iid (toSource -> source) horror = InvestigatorAssignDamage iid source DamageAny 0 horror

assignDamageAndHorror :: Sourceable source => InvestigatorId -> source -> Int -> Int -> Message
assignDamageAndHorror iid (toSource -> source) damage horror = InvestigatorAssignDamage iid source DamageAny damage horror

directDamage :: Sourceable source => InvestigatorId -> source -> Int -> Message
directDamage iid (toSource -> source) damage = InvestigatorDirectDamage iid source damage 0

directHorror :: Sourceable source => InvestigatorId -> source -> Int -> Message
directHorror iid (toSource -> source) horror = InvestigatorDirectDamage iid source 0 horror

findAndDrawEncounterCard :: IsCardMatcher a => InvestigatorId -> a -> Message
findAndDrawEncounterCard investigator cardMatcher = FindAndDrawEncounterCard investigator (toCardMatcher cardMatcher) IncludeDiscard

ready :: Targetable target => target -> Message
ready = Ready . toTarget

chooseEngageEnemy :: Sourceable source => InvestigatorId -> source -> Message
chooseEngageEnemy iid (toSource -> source) = ChooseEngageEnemy iid source Nothing mempty False

search
  :: (Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> target
  -> [(Zone, ZoneReturnStrategy)]
  -> ExtendedCardMatcher
  -> FoundCardsStrategy
  -> Message
search iid (toSource -> source) (toTarget -> target) zones matcher strategy = Do (Search Searching iid source target zones matcher strategy)

lookAt
  :: (Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> target
  -> [(Zone, ZoneReturnStrategy)]
  -> ExtendedCardMatcher
  -> FoundCardsStrategy
  -> Message
lookAt iid (toSource -> source) (toTarget -> target) = Search Looking iid source target

revealing
  :: (Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> target
  -> Zone
  -> Message
revealing iid (toSource -> source) (toTarget -> target) zone = Search Revealing iid source target [(zone, PutBack)] (basic AnyCard) ReturnCards

takeResources :: Sourceable source => InvestigatorId -> source -> Int -> Message
takeResources iid (toSource -> source) n = TakeResources iid n source False

gainResourcesIfCan
  :: (HasGame m, Sourceable source) => InvestigatorId -> source -> Int -> m (Maybe Message)
gainResourcesIfCan iid source n = do
  canGainResources <- can.gain.resources (sourceToFromSource source) iid
  pure $ guard canGainResources $> takeResources iid source n

assignEnemyDamage :: DamageAssignment -> EnemyId -> Message
assignEnemyDamage = flip EnemyDamage

nonAttackEnemyDamage :: Sourceable a => a -> Int -> EnemyId -> Message
nonAttackEnemyDamage source damage enemy = EnemyDamage enemy (nonAttack source damage)

placeDoom :: (Sourceable source, Targetable target) => source -> target -> Int -> Message
placeDoom (toSource -> source) (toTarget -> target) n = PlaceDoom source target n

placeHorror :: (Sourceable source, Targetable target) => source -> target -> Int -> Message
placeHorror (toSource -> source) (toTarget -> target) n = PlaceHorror source target n

addToVictory :: Targetable target => target -> Message
addToVictory (toTarget -> target) = AddToVictory target

-- This is obviously very complicated, but it feels like it shouldn't be
-- However we find the correct message and only remove the amount indicated
cancelDoom :: HasQueue Message m => Target -> Int -> m ()
cancelDoom target n = do
  replaceMessageMatching
    \case
      RunWindow _ [window] -> case windowType window of
        Window.WouldPlaceDoom _ target' _ -> target == target'
        _ -> False
      _ -> False
    \case
      RunWindow iid [window] -> case windowType window of
        Window.WouldPlaceDoom source' target' n' ->
          [RunWindow iid [window {windowType = Window.WouldPlaceDoom source' target' (n' - n)}] | n' - n > 0]
        _ -> error "mismatched"
      _ -> error "mismatched"

  let
    findNewAmount [] = error "mismatches"
    findNewAmount (Do (PlaceDoom _ target' n') : _) | target == target' = n' - n
    findNewAmount (_ : rest) = findNewAmount rest

    replaceWindowTypeDoomAmount m = \case
      Window.WouldPlaceDoom source' target' _ -> Window.WouldPlaceDoom source' target' m
      Window.PlacedDoom source' target' _ -> Window.PlacedDoom source' target' m
      _ -> error "mismatched"

    replaceWindowDoomAmount m Window {..} =
      Window {windowTiming, windowBatchId, windowType = replaceWindowTypeDoomAmount m windowType}

    replaceDoomAmount m = \case
      CheckWindow xs ws -> CheckWindow xs (map (replaceWindowDoomAmount m) ws)
      Do (PlaceTokens source' target' Token.Doom _) | target == target' -> Do (PlaceTokens source' target' Token.Doom m)
      other -> other

  replaceMessageMatching
    \case
      Would _ msgs -> flip any msgs $ \case
        Do (PlaceDoom _ target' _) -> target == target'
        _ -> False
      _ -> False
    \case
      Would batchId msgs ->
        let
          newAmount = findNewAmount msgs
          msgs' = map (replaceDoomAmount newAmount) msgs
         in
          [Would batchId msgs' | newAmount > 0]
      _ -> error "mismatched"

checkDefeated :: (Sourceable source, Targetable target) => source -> target -> Message
checkDefeated source target = CheckDefeated (toSource source) (toTarget target)

placeDoomOnAgenda :: Message
placeDoomOnAgenda = PlaceDoomOnAgenda 1 CanNotAdvance

placeDoomOnAgendaAndCheckAdvance :: Message
placeDoomOnAgendaAndCheckAdvance = PlaceDoomOnAgenda 1 CanAdvance

handleTargetChoice
  :: (Sourceable source, Targetable target) => InvestigatorId -> source -> target -> Message
handleTargetChoice iid (toSource -> source) (toTarget -> target) = HandleTargetChoice iid source target
