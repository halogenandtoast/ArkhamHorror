module Arkham.Message.Lifted (module X, module Arkham.Message.Lifted) where

import Arkham.Ability
import Arkham.Act.Sequence qualified as Act
import Arkham.Act.Types (ActAttrs (actDeckId))
import Arkham.Action (Action)
import Arkham.Agenda.Types (AgendaAttrs (agendaDeckId))
import Arkham.Aspect (IsAspect (..))
import Arkham.Aspect qualified as Msg
import Arkham.Asset.Types (AssetAttrs)
import Arkham.Asset.Uses (UseType)
import Arkham.Attack
import Arkham.Calculation
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue hiding (insertAfterMatching)
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Classes.Query
import Arkham.DamageEffect
import Arkham.Deck (IsDeck (..))
import Arkham.Deck qualified as Deck
import Arkham.Discover as X (IsInvestigate (..))
import Arkham.Discover qualified as Msg
import Arkham.Draw.Types
import Arkham.Effect.Types (EffectBuilder (effectBuilderEffectId), Field (..))
import Arkham.EffectMetadata (EffectMetadata)
import Arkham.Enemy.Creation
import Arkham.Enemy.Helpers qualified as Msg
import Arkham.Enemy.Types (Field (..))
import Arkham.Evade
import Arkham.Evade qualified as Evade
import Arkham.Fight
import Arkham.Fight qualified as Fight
import Arkham.Game.Helpers (getActionsWith, getIsPlayable)
import Arkham.Helpers
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Card (getCardEntityTarget)
import Arkham.Helpers.Effect qualified as Msg
import Arkham.Helpers.Enemy qualified as Msg
import Arkham.Helpers.Investigator (getCanDiscoverClues, withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (getMetaMaybe)
import Arkham.Helpers.Query
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Investigate
import Arkham.Investigate qualified as Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..), Location)
import Arkham.Matcher
import Arkham.Message hiding (story)
import Arkham.Message.Lifted.Queue as X
import Arkham.Modifier
import Arkham.Phase (Phase)
import Arkham.Placement
import Arkham.Prelude hiding (pred)
import Arkham.Projection
import Arkham.Query
import Arkham.Queue
import Arkham.RequestedChaosTokenStrategy
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait (Trait)
import Arkham.Window (Window, WindowType, defaultWindows)
import Arkham.Window qualified as Window
import Arkham.Xp
import Control.Monad.Trans.Class
import Data.Aeson.Key qualified as Aeson

setChaosTokens :: ReverseQueue m => [ChaosTokenFace] -> m ()
setChaosTokens = push . SetChaosTokens

setEncounterDeck :: ReverseQueue m => Deck EncounterCard -> m ()
setEncounterDeck = push . SetEncounterDeck

setAgendaDeck :: ReverseQueue m => [CardDef] -> m ()
setAgendaDeck = genCards >=> push . SetAgendaDeckCards 1

setAgendaDeckN :: ReverseQueue m => Int -> [CardDef] -> m ()
setAgendaDeckN n = genCards >=> push . SetAgendaDeckCards n

setActDeck :: ReverseQueue m => [CardDef] -> m ()
setActDeck = genCards >=> push . SetActDeckCards 1

setActDeckN :: ReverseQueue m => Int -> [CardDef] -> m ()
setActDeckN n = genCards >=> push . SetActDeckCards n

placeSetAsideLocation :: ReverseQueue m => CardDef -> m LocationId
placeSetAsideLocation card = do
  (lid, msg) <- Msg.placeSetAsideLocation card
  push msg
  pure lid

placeSetAsideLocation_ :: ReverseQueue m => CardDef -> m ()
placeSetAsideLocation_ = push <=< Msg.placeSetAsideLocation_

placeSetAsideLocations_ :: ReverseQueue m => [CardDef] -> m ()
placeSetAsideLocations_ = pushAll <=< Msg.placeSetAsideLocations

placeLocationCard
  :: ReverseQueue m => CardDef -> m LocationId
placeLocationCard def = do
  (lid, placement) <- Msg.placeLocationCard def
  push placement
  pure lid

placeLocationCardInGrid
  :: ReverseQueue m => Pos -> CardDef -> m LocationId
placeLocationCardInGrid pos def = do
  (lid, placement) <- Msg.placeLocationCardInGrid pos def
  push placement
  pure lid

placeLocationInGrid
  :: ReverseQueue m => Pos -> Card -> m LocationId
placeLocationInGrid pos card = do
  (lid, placement) <- Msg.placeLocationInGrid pos card
  push placement
  pure lid

placeLocationInGrid_
  :: ReverseQueue m => Pos -> Card -> m ()
placeLocationInGrid_ pos card = void $ placeLocationInGrid pos card

placeLocation
  :: ReverseQueue m => Card -> m LocationId
placeLocation card = do
  (lid, placement) <- Msg.placeLocation card
  push placement
  pure lid

placeLocation_ :: ReverseQueue m => Card -> m ()
placeLocation_ = Msg.placeLocation_ >=> push

placeRandomLocationGroupCards
  :: ReverseQueue m => Text -> [CardDef] -> m ()
placeRandomLocationGroupCards groupName = genCards >=> placeRandomLocationGroup groupName

placeRandomLocationGroupCardsCapture
  :: ReverseQueue m => Text -> [CardDef] -> m [LocationId]
placeRandomLocationGroupCardsCapture groupName = genCards >=> placeRandomLocationGroupCapture groupName

placeRandomLocationGroup
  :: ReverseQueue m => Text -> [Card] -> m ()
placeRandomLocationGroup groupName cards = do
  shuffled <- shuffleM cards
  msgs <- Msg.placeLabeledLocations_ groupName shuffled
  pushAll msgs

placeRandomLocationGroupCapture
  :: ReverseQueue m => Text -> [Card] -> m [LocationId]
placeRandomLocationGroupCapture groupName cards = do
  shuffled <- shuffleM cards
  (lids, msgs) <- Msg.placeLabeledLocations groupName shuffled
  pushAll msgs
  pure lids

placeLabeledLocations_ :: ReverseQueue m => Text -> [Card] -> m ()
placeLabeledLocations_ lbl cards = Msg.pushAllM $ Msg.placeLabeledLocations_ lbl cards

placeLabeledLocations :: ReverseQueue m => Text -> [Card] -> m [LocationId]
placeLabeledLocations lbl cards = Msg.placeLabeledLocations lbl cards >>= \(lids, msgs) -> pushAll msgs $> lids

placeLabeledLocation_ :: ReverseQueue m => Text -> Card -> m ()
placeLabeledLocation_ lbl card = Msg.placeLabeledLocation lbl card >>= \(_, msg) -> push msg

placeLabeledLocation :: ReverseQueue m => Text -> Card -> m LocationId
placeLabeledLocation lbl card = Msg.placeLabeledLocation lbl card >>= \(lid, msg) -> push msg >> pure lid

placeLabeledLocationsFrom
  :: ReverseQueue m => Text -> Int -> [Card] -> m [LocationId]
placeLabeledLocationsFrom lbl n cards = Msg.placeLabeledLocationsFrom lbl n cards >>= \(lids, msgs) -> pushAll msgs $> lids

placeLocationCards
  :: ReverseQueue m => [CardDef] -> m ()
placeLocationCards defs = for_ defs placeLocationCard

placeOneLocationCard
  :: ReverseQueue m => NonEmpty CardDef -> m LocationId
placeOneLocationCard = sample >=> placeLocationCard

placeLocationCardM
  :: ReverseQueue m
  => m CardDef
  -> m LocationId
placeLocationCardM = (>>= placeLocationCard)

reveal :: (AsId location, IdOf location ~ LocationId, ReverseQueue m) => location -> m ()
reveal = push . Msg.RevealLocation Nothing . asId

record :: ReverseQueue m => CampaignLogKey -> m ()
record = push . Record

recordWhen :: ReverseQueue m => Bool -> CampaignLogKey -> m ()
recordWhen True = push . Record
recordWhen False = pure . const ()

recordCount :: ReverseQueue m => CampaignLogKey -> Int -> m ()
recordCount k = push . RecordCount k

remember :: ReverseQueue m => ScenarioLogKey -> m ()
remember = push . Remember

crossOut :: ReverseQueue m => CampaignLogKey -> m ()
crossOut = push . CrossOutRecord

recordSetInsert
  :: (Recordable a, MonoFoldable t, Element t ~ a, ReverseQueue m)
  => CampaignLogKey
  -> t
  -> m ()
recordSetInsert k = push . Msg.recordSetInsert k

recordSetReplace :: ReverseQueue m => CampaignLogKey -> SomeRecorded -> SomeRecorded -> m ()
recordSetReplace k v v' = push $ Msg.RecordSetReplace k v v'

incrementRecordCount :: ReverseQueue m => CampaignLogKey -> Int -> m ()
incrementRecordCount key = push . IncrementRecordCount key

story :: ReverseQueue m => FlavorText -> m ()
story flavor = do
  players <- allPlayers
  push $ Msg.story players flavor

storyWhen :: ReverseQueue m => Bool -> FlavorText -> m ()
storyWhen cond flavor = when cond do
  players <- allPlayers
  push $ Msg.story players flavor

storyWithCard :: ReverseQueue m => CardDef -> FlavorText -> m ()
storyWithCard cardDef flavor = do
  players <- allPlayers
  push $ Msg.storyWithCards [cardDef] players flavor

storyWithCards :: ReverseQueue m => [CardDef] -> FlavorText -> m ()
storyWithCards cardDefs flavor = do
  players <- allPlayers
  push $ Msg.storyWithCards cardDefs players flavor

storyOnly :: ReverseQueue m => [InvestigatorId] -> FlavorText -> m ()
storyOnly [] _ = pure ()
storyOnly iids flavor = do
  players <- traverse getPlayer iids
  push $ Msg.story players flavor

storyWithChooseOne :: ReverseQueue m => FlavorText -> [UI Message] -> m ()
storyWithChooseOne flavor choices = do
  players <- allPlayers
  lead <- getLeadPlayer
  push $ Msg.storyWithChooseOne lead players flavor choices

sufferTrauma :: ReverseQueue m => InvestigatorId -> Int -> Int -> m ()
sufferTrauma iid physical mental = push $ SufferTrauma iid physical mental

sufferMentalTrauma :: ReverseQueue m => InvestigatorId -> Int -> m ()
sufferMentalTrauma iid mental = sufferTrauma iid 0 mental

sufferPhysicalTrauma :: ReverseQueue m => InvestigatorId -> Int -> m ()
sufferPhysicalTrauma iid physical = sufferTrauma iid physical 0

gainXp
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Text -> Int -> m ()
gainXp iid (toSource -> source) from xp = do
  let report = XpBreakdown [InvestigatorGainXp iid $ XpDetail XpFromCardEffect ("$" <> from) xp]
  push $ ReportXp report
  push $ GainXP iid source xp

allGainXpWithBonus :: (ReverseQueue m, Sourceable source) => source -> XpBonus -> m ()
allGainXpWithBonus source xp = do
  push . ReportXp =<< generateXpReport xp
  pushAll =<< toGainXp source (getXpWithBonus xp.value)

allGainXpWithBonus' :: (ReverseQueue m, Sourceable source) => source -> XpBonus -> m Int
allGainXpWithBonus' source xp = do
  push . ReportXp =<< generateXpReport xp
  (initial, details) <- getXpWithBonus' xp.value
  pushAll =<< toGainXp source (pure details)
  pure initial

allGainXp' :: (ReverseQueue m, Sourceable source) => source -> m Int
allGainXp' source = do
  (initial, details) <- getXp'
  push . ReportXp =<< generateXpReport NoBonus
  pushAll =<< toGainXp source (pure details)
  pure initial

allGainXp :: (ReverseQueue m, Sourceable source) => source -> m ()
allGainXp = void . allGainXp'

allGainXpWith
  :: (ReverseQueue m, Sourceable source) => source -> (InvestigatorId -> [XpEntry]) -> m ()
allGainXpWith source f = do
  (pairs', additionalEntries) <-
    foldMap
      (\(iid, n) -> let entries = f iid in ([(iid, n + sum (map (.amount) entries))], entries))
      <$> getXp
  push . ReportXp . (<> XpBreakdown additionalEntries) =<< generateXpReport NoBonus
  pushAll =<< toGainXp source (pure pairs')

interludeXpAll :: ReverseQueue m => XpBonus -> m ()
interludeXpAll xp = do
  investigatorIds <- allInvestigators
  push
    $ ReportXp
    $ XpBreakdown
      [ InvestigatorGainXp iid $ XpDetail XpBonus txt n | WithBonus txt n <- xp.flatten, iid <- investigatorIds
      ]
  for_ investigatorIds \iid -> do
    push $ GainXP iid CampaignSource xp.value

interludeXp :: ReverseQueue m => InvestigatorId -> XpBonus -> m ()
interludeXp iid xp = do
  pushAll
    [ ReportXp
        $ XpBreakdown
          [InvestigatorGainXp iid $ XpDetail XpBonus txt n | WithBonus txt n <- xp.flatten]
    , GainXP iid CampaignSource xp.value
    ]

endOfScenario :: ReverseQueue m => m ()
endOfScenario = push $ EndOfGame Nothing

endOfScenarioThen :: ReverseQueue m => CampaignStep -> m ()
endOfScenarioThen = push . EndOfGame . Just

assignDamage
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
assignDamage iid (toSource -> source) damage = push $ Msg.assignDamage iid source damage

assignHorror
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
assignHorror iid (toSource -> source) horror = push $ Msg.assignHorror iid source horror

assignDamageAndHorror
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> Int -> m ()
assignDamageAndHorror iid (toSource -> source) damage horror = push $ Msg.assignDamageAndHorror iid source damage horror

directDamage :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
directDamage iid source = push . Msg.directDamage iid source

directHorror :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
directHorror iid source = push . Msg.directHorror iid source

findAndDrawEncounterCard
  :: (ReverseQueue m, IsCardMatcher a) => InvestigatorId -> a -> m ()
findAndDrawEncounterCard iid matcher = push $ Msg.findAndDrawEncounterCard iid matcher

findAndDrawEncounterCardFromEncounterDeck
  :: (ReverseQueue m, IsCardMatcher a) => InvestigatorId -> a -> m ()
findAndDrawEncounterCardFromEncounterDeck iid matcher = push $ Msg.findAndDrawEncounterCardFromEncounterDeck iid matcher

findEncounterCard
  :: forall cardMatcher target m
   . (ReverseQueue m, Targetable target, IsCardMatcher cardMatcher)
  => InvestigatorId
  -> target
  -> cardMatcher
  -> m ()
findEncounterCard iid target cardMatcher = findEncounterCardIn iid target cardMatcher [FromEncounterDeck, FromEncounterDiscard]

findEncounterCardIn
  :: forall cardMatcher target m
   . (ReverseQueue m, Targetable target, IsCardMatcher cardMatcher)
  => InvestigatorId
  -> target
  -> cardMatcher
  -> [ScenarioZone]
  -> m ()
findEncounterCardIn iid target cardMatcher scenarioZones =
  push
    $ Msg.FindEncounterCard
      iid
      (toTarget target)
      scenarioZones
      (toCardMatcher cardMatcher)

beginSkillTest
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType.SkillType
  -> GameCalculation
  -> m ()
beginSkillTest sid iid source target sType n = push $ Msg.beginSkillTest sid iid source target sType n

gameOverIf :: ReverseQueue m => Bool -> m ()
gameOverIf t = when t gameOver

gameOver :: ReverseQueue m => m ()
gameOver = push GameOver

kill :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
kill (toSource -> source) = push . InvestigatorKilled source

drivenInsane :: ReverseQueue m => InvestigatorId -> m ()
drivenInsane = push . DrivenInsane

killRemaining
  :: (Sourceable source, ReverseQueue m) => source -> m [InvestigatorId]
killRemaining (toSource -> source) = do
  remaining <- select UneliminatedInvestigator
  resigned <- select ResignedInvestigator
  for_ remaining $ kill source
  gameOverIf (null resigned)
  pure remaining

class FetchCard a where
  fetchCard :: (HasCallStack, ReverseQueue m) => a -> m Card

instance FetchCard CardDef where
  fetchCard def = maybe (genCard def) pure =<< maybeGetSetAsideCard def

instance FetchCard Card where
  fetchCard = pure

instance FetchCard PlayerCard where
  fetchCard = pure . toCard

addCampaignCardToDeck
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, ReverseQueue m, FetchCard card)
  => investigator
  -> card
  -> m ()
addCampaignCardToDeck investigator card = do
  card' <- fetchCard card
  push $ Msg.AddCampaignCardToDeck (asId investigator) card'

addCampaignCardToDeckChoice :: (FetchCard card, ReverseQueue m) => [InvestigatorId] -> card -> m ()
addCampaignCardToDeckChoice choices card = do
  lead <- getLeadPlayer
  card' <- fetchCard card
  push $ Msg.addCampaignCardToDeckChoice lead choices card'

forceAddCampaignCardToDeckChoice
  :: (FetchCard card, ReverseQueue m) => [InvestigatorId] -> card -> m ()
forceAddCampaignCardToDeckChoice choices card = do
  lead <- getLeadPlayer
  card' <- fetchCard card
  push $ Msg.forceAddCampaignCardToDeckChoice lead choices card'

removeCardFromDeckForCampaign
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, IsCard card, ReverseQueue m)
  => investigator
  -> card
  -> m ()
removeCardFromDeckForCampaign investigator card = push $ Msg.RemoveCardFromDeckForCampaign (asId investigator) (toCardId card)

defeatEnemy :: (ReverseQueue m, Sourceable source) => EnemyId -> InvestigatorId -> source -> m ()
defeatEnemy enemyId investigatorId = Msg.defeatEnemy enemyId investigatorId >=> pushAll

createAsset :: (ReverseQueue m, IsCard card) => card -> m AssetId
createAsset card = do
  assetId <- getId
  push $ CreateAssetAt assetId (toCard card) Unplaced
  pure assetId

createEnemyEngagedWithPrey :: ReverseQueue m => Card -> m EnemyId
createEnemyEngagedWithPrey c = do
  creation <- Msg.createEnemy c SpawnEngagedWithPrey
  push $ CreateEnemy creation
  pure $ enemyCreationEnemyId creation

createEnemyEngagedWithPrey_ :: ReverseQueue m => Card -> m ()
createEnemyEngagedWithPrey_ = void . createEnemyEngagedWithPrey

createEnemyAt_
  :: (ReverseQueue m, IsCard card, AsId location, IdOf location ~ LocationId) => card -> location -> m ()
createEnemyAt_ c location = push =<< Msg.createEnemyAt_ (toCard c) (asId location) Nothing

createEnemyAt
  :: (ReverseQueue m, IsCard card) => card -> LocationId -> m EnemyId
createEnemyAt c lid = do
  (enemyId, msg) <- Msg.createEnemyAt (toCard c) lid Nothing
  push msg
  pure enemyId

createEnemyAtLocationMatching_ :: (ReverseQueue m, IsCard card) => card -> LocationMatcher -> m ()
createEnemyAtLocationMatching_ c = Msg.pushM . Msg.createEnemyAtLocationMatching_ (toCard c)

createEnemyAtLocationMatching
  :: (ReverseQueue m, IsCard card) => card -> LocationMatcher -> m EnemyId
createEnemyAtLocationMatching c matcher = do
  (eid, msg) <- Msg.createEnemyAtLocationMatching (toCard c) matcher
  Msg.push msg
  pure eid

createSetAsideEnemy
  :: (ReverseQueue m, IsEnemyCreationMethod creation) => CardDef -> creation -> m EnemyId
createSetAsideEnemy def creation = createSetAsideEnemyWith def creation id

createSetAsideEnemy_
  :: (ReverseQueue m, IsEnemyCreationMethod creation) => CardDef -> creation -> m ()
createSetAsideEnemy_ def creation = createSetAsideEnemyWith_ def creation id

createSetAsideEnemyWith
  :: (ReverseQueue m, IsEnemyCreationMethod creation)
  => CardDef
  -> creation
  -> (EnemyCreation Message -> EnemyCreation Message)
  -> m EnemyId
createSetAsideEnemyWith def creation f = do
  card <- getSetAsideCard def
  createEnemyWith card creation f

createSetAsideEnemyWith_
  :: (ReverseQueue m, IsEnemyCreationMethod creation)
  => CardDef
  -> creation
  -> (EnemyCreation Message -> EnemyCreation Message)
  -> m ()
createSetAsideEnemyWith_ def creation f = void $ createSetAsideEnemyWith def creation f

createEnemyWith
  :: (ReverseQueue m, IsCard card, IsEnemyCreationMethod creation)
  => card
  -> creation
  -> (EnemyCreation Message -> EnemyCreation Message)
  -> m EnemyId
createEnemyWith card creation f = do
  msg <- Msg.createEnemy card creation
  push $ toMessage (f msg)
  pure msg.enemy

createEnemyWith_
  :: (ReverseQueue m, IsCard card, IsEnemyCreationMethod creation)
  => card
  -> creation
  -> (EnemyCreation Message -> EnemyCreation Message)
  -> m ()
createEnemyWith_ card creation f = void $ createEnemyWith card creation f

createEnemyCard_
  :: (ReverseQueue m, FetchCard card, IsEnemyCreationMethod creation)
  => card
  -> creation
  -> m ()
createEnemyCard_ fetch creation = fetchCard fetch >>= (`createEnemy_` creation)

createEnemy_
  :: (ReverseQueue m, IsCard card, IsEnemyCreationMethod creation)
  => card
  -> creation
  -> m ()
createEnemy_ card creation = void $ createEnemyWith card creation id

createEnemy
  :: (ReverseQueue m, IsCard card, IsEnemyCreationMethod creation)
  => card
  -> creation
  -> m EnemyId
createEnemy card creation = createEnemyWith card creation id

spawnEnemy :: (ReverseQueue m, IsCard card) => card -> m EnemyId
spawnEnemy card = createEnemyWith card () id

spawnEnemy_ :: (ReverseQueue m, IsCard card) => card -> m ()
spawnEnemy_ = void . spawnEnemy

spawnEnemyAt_
  :: (ReverseQueue m, IsCard card, AsId location, IdOf location ~ LocationId) => card -> location -> m ()
spawnEnemyAt_ card location = push $ SpawnEnemyAt (toCard card) (asId location)

setAsideCards :: ReverseQueue m => [CardDef] -> m ()
setAsideCards = genCards >=> push . Msg.SetAsideCards

setCardAside :: ReverseQueue m => Card -> m ()
setCardAside = push . Msg.SetAsideCards . (: [])

addChaosToken :: ReverseQueue m => ChaosTokenFace -> m ()
addChaosToken = push . AddChaosToken

removeChaosToken :: ReverseQueue m => ChaosTokenFace -> m ()
removeChaosToken = push . RemoveChaosToken

removeAllChaosTokens :: ReverseQueue m => ChaosTokenFace -> m ()
removeAllChaosTokens = push . RemoveAllChaosTokens

removeCampaignCard :: (HasCardDef a, ReverseQueue m) => a -> m ()
removeCampaignCard (toCardDef -> def) = do
  mOwner <- getOwner def
  for_ mOwner \owner ->
    push $ RemoveCampaignCardFromDeck owner def

placeClues
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeClues source target n = push $ PlaceClues (toSource source) (toTarget target) n

placeDoom
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeDoom source target n = push $ PlaceDoom (toSource source) (toTarget target) n

removeDoom
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
removeDoom source target n = push $ RemoveDoom (toSource source) (toTarget target) n

removeAllDoom :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
removeAllDoom source target = push $ RemoveAllDoom (toSource source) (toTarget target)

placeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
placeTokens source lid token n = push $ PlaceTokens (toSource source) (toTarget lid) token n

removeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
removeTokens source lid token n = push $ RemoveTokens (toSource source) (toTarget lid) token n

moveTokens
  :: (ReverseQueue m, Sourceable source, Sourceable from, Targetable destination)
  => source
  -> from
  -> destination
  -> Token
  -> Int
  -> m ()
moveTokens source from destination token n = push $ Msg.MoveTokens (toSource source) (toSource from) (toTarget destination) token n

sourceTokens :: (ReverseQueue m, Sourceable source) => source -> m Tokens
sourceTokens source = case toSource source of
  EnemySource eid -> field EnemyTokens eid
  _ ->
    error
      "This bug is because I need to lookup the tokens for a source, but I was too lazy to impelement them all"

moveAllTokens
  :: ( ReverseQueue m
     , Sourceable source
     , Sourceable from
     , Targetable destination
     )
  => source
  -> from
  -> destination
  -> Token
  -> m ()
moveAllTokens source from destination token = do
  m <- findWithDefault 0 token <$> sourceTokens from
  push $ Msg.MoveTokens (toSource source) (toSource from) (toTarget destination) token m

moveTokensNoDefeated
  :: (ReverseQueue m, Sourceable source, Sourceable from, Targetable destination)
  => source
  -> from
  -> destination
  -> Token
  -> Int
  -> m ()
moveTokensNoDefeated source from destination token n = push $ Msg.MoveTokensNoDefeated (toSource source) (toSource from) (toTarget destination) token n

drawAnotherChaosToken :: ReverseQueue m => InvestigatorId -> m ()
drawAnotherChaosToken = push . DrawAnotherChaosToken

assignEnemyDamage :: ReverseQueue m => DamageAssignment -> EnemyId -> m ()
assignEnemyDamage assignment = push . Msg.assignEnemyDamage assignment

eachInvestigator :: ReverseQueue m => (InvestigatorId -> m ()) -> m ()
eachInvestigator f = do
  investigators <- getInvestigators
  for_ investigators f

forInvestigator :: ReverseQueue m => InvestigatorId -> Message -> m ()
forInvestigator iid msg = push $ ForInvestigator iid msg

selectEach :: (Query a, HasGame m) => a -> (QueryElement a -> m ()) -> m ()
selectEach matcher f = select matcher >>= traverse_ f

selectForEach :: (Query a, ReverseQueue m) => a -> (QueryElement a -> m b) -> m [b]
selectForEach matcher f = select matcher >>= traverse f

selectForToSnd
  :: (Query a, ReverseQueue m) => a -> (QueryElement a -> m b) -> m [(QueryElement a, b)]
selectForToSnd matcher f = select matcher >>= (`forToSnd` f)

selectWithNonNull
  :: (HasCallStack, Query a, ReverseQueue m) => a -> ([QueryElement a] -> m ()) -> m ()
selectWithNonNull matcher f = do
  xs <- select matcher
  unless (null xs) (f xs)

advanceAgendaDeck :: ReverseQueue m => AgendaAttrs -> m ()
advanceAgendaDeck attrs = push $ AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)

advanceActDeck :: ReverseQueue m => ActAttrs -> m ()
advanceActDeck attrs = push $ AdvanceActDeck (actDeckId attrs) (toSource attrs)

advanceToAct :: ReverseQueue m => ActAttrs -> CardDef -> Act.ActSide -> m ()
advanceToAct attrs nextAct actSide = push $ AdvanceToAct (actDeckId attrs) nextAct actSide (toSource attrs)

shuffleEncounterDiscardBackIn :: ReverseQueue m => m ()
shuffleEncounterDiscardBackIn = push ShuffleEncounterDiscardBackIn

placeDoomOnAgenda :: ReverseQueue m => Int -> m ()
placeDoomOnAgenda n = push $ PlaceDoomOnAgenda n CanNotAdvance

placeDoomOnAgendaAndCheckAdvance :: ReverseQueue m => Int -> m ()
placeDoomOnAgendaAndCheckAdvance n = push $ PlaceDoomOnAgenda n CanAdvance

revertAgenda :: (ReverseQueue m, AsId a, IdOf a ~ AgendaId) => a -> m ()
revertAgenda a = push $ RevertAgenda (asId a)

chooseOrRunOne :: (ReverseQueue m, HasCallStack) => InvestigatorId -> [UI Message] -> m ()
chooseOrRunOne iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOrRunOne player msgs

continue :: ReverseQueue m => InvestigatorId -> [Message] -> m ()
continue iid = prompt iid "Continue"

prompt :: ReverseQueue m => InvestigatorId -> Text -> [Message] -> m ()
prompt iid lbl msgs = Arkham.Message.Lifted.chooseOne iid [Label lbl msgs]

prompt_ :: ReverseQueue m => InvestigatorId -> Text -> m ()
prompt_ iid lbl = Arkham.Message.Lifted.chooseOne iid [Label lbl []]

aspect
  :: (ReverseQueue m, IsAspect a b, IsMessage b, Sourceable source)
  => InvestigatorId
  -> source
  -> a
  -> m b
  -> m ()
aspect iid source a action = Msg.aspect iid source a action >>= pushAll . Msg.leftOr

choose :: ReverseQueue m => InvestigatorId -> UI Message -> m ()
choose iid msg = Arkham.Message.Lifted.chooseOne iid [msg]

questionLabel :: ReverseQueue m => Text -> InvestigatorId -> Question Message -> m ()
questionLabel lbl iid q = do
  pid <- getPlayer iid
  push $ Ask pid (QuestionLabel lbl Nothing q)

chooseOne :: (HasCallStack, ReverseQueue m) => InvestigatorId -> [UI Message] -> m ()
chooseOne iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOne player msgs

chooseOneFromEach :: (HasCallStack, ReverseQueue m) => InvestigatorId -> [[UI Message]] -> m ()
chooseOneFromEach iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOneFromEach player msgs

chooseSome :: ReverseQueue m => InvestigatorId -> Text -> [UI Message] -> m ()
chooseSome iid done msgs = do
  player <- getPlayer iid
  push $ Msg.chooseSome player done msgs

chooseSome1 :: ReverseQueue m => InvestigatorId -> Text -> [UI Message] -> m ()
chooseSome1 iid done msgs = do
  player <- getPlayer iid
  push $ Msg.chooseSome1 player done msgs

selectOneToHandle
  :: (HasCallStack, ReverseQueue m, Targetable (QueryElement matcher), Query matcher, Sourceable source)
  => InvestigatorId
  -> source
  -> matcher
  -> m ()
selectOneToHandle iid source matcher =
  select matcher >>= \results -> if notNull results then chooseOneToHandle iid source results else pure ()

selectOneToHandleWith
  :: (HasCallStack, ReverseQueue m, Targetable (QueryElement matcher), Query matcher, Sourceable source)
  => InvestigatorId
  -> source
  -> Message
  -> matcher
  -> m ()
selectOneToHandleWith iid source msg matcher =
  select matcher >>= \results -> if notNull results then chooseOneToHandleWith iid source results msg else pure ()

chooseOneToHandle
  :: (HasCallStack, ReverseQueue m, Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> [target]
  -> m ()
chooseOneToHandle iid source targets =
  Arkham.Message.Lifted.chooseOne iid
    $ targetLabels targets
    $ only
    . Msg.handleTargetChoice iid source

chooseOneToHandleWith
  :: (HasCallStack, ReverseQueue m, Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> [target]
  -> Message
  -> m ()
chooseOneToHandleWith iid source targets msg =
  Arkham.Message.Lifted.chooseOne iid
    $ targetLabels targets \target -> [Msg.handleTargetChoice iid source target, msg]

selectOrRunOneToHandle
  :: (HasCallStack, ReverseQueue m, Targetable (QueryElement matcher), Query matcher, Sourceable source)
  => InvestigatorId
  -> source
  -> matcher
  -> m ()
selectOrRunOneToHandle iid source matcher =
  select matcher >>= \results -> if notNull results then chooseOrRunOneToHandle iid source results else pure ()

chooseOrRunOneToHandle
  :: (ReverseQueue m, Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> [target]
  -> m ()
chooseOrRunOneToHandle iid source targets =
  Arkham.Message.Lifted.chooseOrRunOne iid
    $ targetLabels targets
    $ only
    . Msg.handleTargetChoice iid source

handleOneAtATime
  :: (ReverseQueue m, Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> [target]
  -> m ()
handleOneAtATime iid source targets =
  Arkham.Message.Lifted.chooseOneAtATime iid
    $ targetLabels targets
    $ only
    . Msg.handleTargetChoice iid source

handleOneAtATimeSelect
  :: (ReverseQueue m, Sourceable source, Query matcher, Targetable (QueryElement matcher))
  => InvestigatorId
  -> source
  -> matcher
  -> m ()
handleOneAtATimeSelect iid source = handleOneAtATime iid source <=< select

handleN
  :: (ReverseQueue m, Targetable target, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> [target]
  -> m ()
handleN iid source n targets =
  Arkham.Message.Lifted.chooseN iid n
    $ targetLabels targets
    $ only
    . Msg.handleTargetChoice iid source

handleNSelect
  :: (ReverseQueue m, Sourceable source, Query matcher, Targetable (QueryElement matcher))
  => InvestigatorId
  -> source
  -> Int
  -> matcher
  -> m ()
handleNSelect iid source n matcher = handleN iid source n =<< select matcher

chooseUpToN :: ReverseQueue m => InvestigatorId -> Int -> Text -> [UI Message] -> m ()
chooseUpToN iid n label msgs = do
  player <- getPlayer iid
  push $ Msg.chooseUpToN player n label msgs

chooseOneAtATime :: ReverseQueue m => InvestigatorId -> [UI Message] -> m ()
chooseOneAtATime iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOneAtATime player msgs

chooseOrRunOneAtATime :: ReverseQueue m => InvestigatorId -> [UI Message] -> m ()
chooseOrRunOneAtATime iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOrRunOneAtATime player msgs

chooseOneDropDown :: ReverseQueue m => InvestigatorId -> [(Text, Message)] -> m ()
chooseOneDropDown iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOneDropDown player msgs

chooseAmounts
  :: (Targetable target, ReverseQueue m)
  => InvestigatorId
  -> Text
  -> AmountTarget
  -> [(Text, (Int, Int))]
  -> target
  -> m ()
chooseAmounts iid label total choiceMap target = do
  player <- getPlayer iid
  Msg.pushM $ Msg.chooseAmounts player label total choiceMap target

chooseAmount
  :: (Targetable target, ReverseQueue m)
  => InvestigatorId
  -> Text
  -> Text
  -> Int
  -> Int
  -> target
  -> m ()
chooseAmount iid label choiceLabel minVal maxVal target = do
  player <- getPlayer iid
  Msg.pushM
    $ Msg.chooseAmounts player label (MaxAmountTarget maxVal) [(choiceLabel, (minVal, maxVal))] target

chooseN :: ReverseQueue m => InvestigatorId -> Int -> [UI Message] -> m ()
chooseN iid n msgs = do
  player <- getPlayer iid
  push $ Msg.chooseN player n msgs

chooseOrRunN :: ReverseQueue m => InvestigatorId -> Int -> [UI Message] -> m ()
chooseOrRunN iid n msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOrRunN player n msgs

addToHand
  :: (ReverseQueue m, MonoFoldable cards, Element cards ~ card, IsCard card)
  => InvestigatorId
  -> cards
  -> m ()
addToHand iid (toList -> cards) = do
  for_ cards obtainCard
  push $ AddToHand iid (map toCard cards)

addToHandQuiet
  :: (ReverseQueue m, MonoFoldable cards, Element cards ~ card, IsCard card)
  => InvestigatorId
  -> cards
  -> m ()
addToHandQuiet iid (toList -> cards) = do
  for_ cards obtainCard
  push $ AddToHandQuiet iid (map toCard cards)

returnToHand :: (Targetable a, ReverseQueue m) => InvestigatorId -> a -> m ()
returnToHand iid = push . ReturnToHand iid . toTarget

addToVictory :: (ReverseQueue m, Targetable target) => target -> m ()
addToVictory = push . AddToVictory . toTarget

createCardEffect
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => CardDef
  -> Maybe (EffectMetadata Window Message)
  -> source
  -> target
  -> m ()
createCardEffect def mMeta source target = push =<< Msg.createCardEffect def mMeta source target

createCardEffectCapture
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => CardDef
  -> Maybe (EffectMetadata Window Message)
  -> source
  -> target
  -> m EffectId
createCardEffectCapture def mMeta source target = do
  effectId <- getRandom
  builder <- Msg.makeEffectBuilder def.cardCode mMeta source target
  push $ Msg.CreateEffect builder {effectBuilderEffectId = Just effectId}
  pure effectId

phaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
phaseModifier source target modifier = Msg.pushM $ Msg.phaseModifier source target modifier

phaseModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
phaseModifiers source target modifiers = Msg.pushM $ Msg.phaseModifiers source target modifiers

resolutionModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
resolutionModifier source target modifier = Msg.pushM $ Msg.resolutionModifier source target modifier

gameModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
gameModifier source target modifier = Msg.pushM $ Msg.gameModifier source target modifier

gameModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
gameModifiers source target = traverse_ (gameModifier source target)

nextTurnModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m ()
nextTurnModifier iid source target modifier = Msg.pushM $ Msg.nextTurnModifier iid source target modifier

nextTurnModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> [ModifierType]
  -> m ()
nextTurnModifiers iid source target modifiers = Msg.pushM $ Msg.nextTurnModifiers iid source target modifiers

flipOver
  :: (ReverseQueue m, Sourceable a, Targetable a) => InvestigatorId -> a -> m ()
flipOver iid a = push $ Msg.Flip iid (toSource a) (toTarget a)

flipOverBy
  :: (ReverseQueue m, Sourceable source, Targetable target) => InvestigatorId -> source -> target -> m ()
flipOverBy iid source target = push $ Msg.Flip iid (toSource source) (toTarget target)

nextPhaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m ()
nextPhaseModifier phase source target modifier = Msg.pushM $ Msg.nextPhaseModifier phase source target modifier

nextPhaseModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => Phase
  -> source
  -> target
  -> [ModifierType]
  -> m ()
nextPhaseModifiers phase source target modifiers = Msg.pushM $ Msg.nextPhaseModifiers phase source target modifiers

endOfPhaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m ()
endOfPhaseModifier phase source target modifier = Msg.pushM $ Msg.endOfPhaseModifier phase source target modifier

endOfNextPhaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m ()
endOfNextPhaseModifier phase source target modifier = Msg.pushM $ Msg.endOfNextPhaseModifier phase source target modifier

roundModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
roundModifier source target modifier = Msg.pushM $ Msg.roundModifier source target modifier

roundModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
roundModifiers source target modifiers = Msg.pushM $ Msg.roundModifiers source target modifiers

modifySkillTest
  :: (Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId, ReverseQueue m)
  => source
  -> investigator
  -> [ModifierType]
  -> m ()
modifySkillTest source investigator mods = whenJustM Msg.getSkillTestId \sid -> skillTestModifiers sid source (asId investigator) mods

skillTestModifiers
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => SkillTestId
  -> source
  -> target
  -> [ModifierType]
  -> m ()
skillTestModifiers sid (toSource -> source) (toTarget -> target) mods =
  Msg.pushM $ Msg.skillTestModifiers sid source target mods

turnModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m ()
turnModifier iid source target modifier = Msg.pushM $ Msg.turnModifier iid source target modifier

turnModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> [ModifierType]
  -> m ()
turnModifiers iid source target modifiers = Msg.pushM $ Msg.turnModifiers iid source target modifiers

setupModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
setupModifier source target modifier = Msg.pushM $ Msg.setupModifier source target modifier

scenarioSetupModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => ScenarioId
  -> source
  -> target
  -> ModifierType
  -> m ()
scenarioSetupModifier scenarioId source target modifier = Msg.pushM $ Msg.scenarioSetupModifier scenarioId source target modifier

revelationModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> TreacheryId
  -> ModifierType
  -> m ()
revelationModifier (toSource -> source) (toTarget -> target) tid modifier =
  Msg.pushM $ Msg.revelationModifier source target tid modifier

revelationModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> TreacheryId
  -> [ModifierType]
  -> m ()
revelationModifiers (toSource -> source) (toTarget -> target) tid modifiers =
  Msg.pushM $ Msg.revelationModifiers source target tid modifiers

skillTestModifier
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => SkillTestId
  -> source
  -> target
  -> ModifierType
  -> m ()
skillTestModifier sid (toSource -> source) (toTarget -> target) x =
  Msg.pushM $ Msg.skillTestModifier sid source target x

nextSkillTestModifier
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> ModifierType
  -> m ()
nextSkillTestModifier (toSource -> source) (toTarget -> target) x =
  Msg.pushM $ Msg.nextSkillTestModifier source target x

nextSkillTestModifiers
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> [ModifierType]
  -> m ()
nextSkillTestModifiers (toSource -> source) (toTarget -> target) x =
  Msg.pushM $ Msg.nextSkillTestModifiers source target x

searchModifier
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> ModifierType
  -> m ()
searchModifier (toSource -> source) (toTarget -> target) modifier =
  Msg.pushM $ Msg.searchModifier source target modifier

chooseFightEnemy
  :: (ReverseQueue m, Sourceable source) => SkillTestId -> InvestigatorId -> source -> m ()
chooseFightEnemy sid iid = mkChooseFight sid iid >=> push . toMessage

chooseFightEnemyWithModifiers
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> [ModifierType]
  -> m ()
chooseFightEnemyWithModifiers sid iid source mods = do
  skillTestModifiers sid source iid mods
  push . toMessage =<< mkChooseFight sid iid source

chooseFightEnemyEdit
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> (ChooseFight -> ChooseFight)
  -> m ()
chooseFightEnemyEdit sid iid source f = mkChooseFight sid iid source >>= push . toMessage . f

chooseFightEnemyWithSkillChoice
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> [SkillType]
  -> m ()
chooseFightEnemyWithSkillChoice sid iid source skillTypes = do
  fight <- mkChooseFight sid iid source
  let using = toMessage . (`Fight.withSkillType` fight)
  Arkham.Message.Lifted.chooseOne
    iid
    [Label ("Use " <> format sType) [using sType] | sType <- skillTypes]

chooseFightEnemyMatch
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> EnemyMatcher
  -> m ()
chooseFightEnemyMatch sid iid source = mkChooseFightMatch sid iid source >=> push . toMessage

chooseFightEnemyMatchEdit
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> EnemyMatcher
  -> (ChooseFight -> ChooseFight)
  -> m ()
chooseFightEnemyMatchEdit sid iid source mtch f = mkChooseFightMatch sid iid source mtch >>= push . toMessage . f

chooseEvadeEnemy
  :: (ReverseQueue m, Sourceable source) => SkillTestId -> InvestigatorId -> source -> m ()
chooseEvadeEnemy sid iid = mkChooseEvade sid iid >=> push . toMessage

chooseEvadeEnemyEdit
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> (ChooseEvade -> ChooseEvade)
  -> m ()
chooseEvadeEnemyEdit sid iid source f = mkChooseEvade sid iid source >>= push . toMessage . f

chooseEvadeEnemyWithSkillChoice
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> [SkillType]
  -> m ()
chooseEvadeEnemyWithSkillChoice sid iid source skillTypes = do
  evade <- mkChooseEvade sid iid source
  let using = toMessage . (`Evade.withSkillType` evade)
  Arkham.Message.Lifted.chooseOne
    iid
    [Label ("Use " <> format sType) [using sType] | sType <- skillTypes]

chooseEvadeEnemyMatch
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> EnemyMatcher
  -> m ()
chooseEvadeEnemyMatch sid iid source = mkChooseEvadeMatch sid iid source >=> push . toMessage

investigateWithSkillChoice
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> [SkillType]
  -> m ()
investigateWithSkillChoice sid iid source skillTypes = do
  inv <- mkInvestigate sid iid source
  let using = toMessage . (`Investigate.withSkillType` inv)
  Arkham.Message.Lifted.chooseOne
    iid
    [Label ("Use " <> format sType) [using sType] | sType <- skillTypes]

investigate
  :: (ReverseQueue m, Sourceable source)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> m ()
investigate sid iid source = push . toMessage =<< mkInvestigate sid iid source

mapQueue :: (MonadTrans t, HasQueue Message m) => (Message -> Message) -> t m ()
mapQueue = lift . Msg.mapQueue

toDiscardBy
  :: (ReverseQueue m, Sourceable source, Targetable target) => InvestigatorId -> source -> target -> m ()
toDiscardBy iid source target = push $ Msg.toDiscardBy iid source target

toDiscard
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
toDiscard source target = push $ Msg.toDiscard source target

putCardIntoPlay :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
putCardIntoPlay iid card = push $ Msg.putCardIntoPlayWithAdditionalCosts iid card

putCampaignCardIntoPlay :: (ReverseQueue m, HasCardDef def) => InvestigatorId -> def -> m ()
putCampaignCardIntoPlay iid def = push $ PutCampaignCardIntoPlay iid (toCardDef def)

putOnBottomOfDeck
  :: (ReverseQueue m, IsDeck deck, Targetable target) => InvestigatorId -> deck -> target -> m ()
putOnBottomOfDeck iid deck target = push $ PutOnBottomOfDeck iid (toDeck deck) (toTarget target)

putCardOnBottomOfDeck
  :: (ReverseQueue m, IsDeck deck, IsCard card) => InvestigatorId -> deck -> card -> m ()
putCardOnBottomOfDeck iid deck card = push $ PutCardOnBottomOfDeck iid (toDeck deck) (toCard card)

gainResourcesIfCan
  :: (ReverseQueue m, Sourceable source, AsId a, IdOf a ~ InvestigatorId) => a -> source -> Int -> m ()
gainResourcesIfCan a source n = do
  mmsg <- Msg.gainResourcesIfCan a source n
  for_ mmsg push

loseResources :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
loseResources iid source n = push $ Msg.LoseResources iid (toSource source) n

drawEncounterCard :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
drawEncounterCard i source = push $ Msg.drawEncounterCards i source 1

drawEncounterCards :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
drawEncounterCards i source n = push $ Msg.drawEncounterCards i source n

drawCardsIfCan
  :: (ReverseQueue m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> source
  -> Int
  -> m ()
drawCardsIfCan iid source n = do
  when (n > 0) do
    mmsg <- Msg.drawCardsIfCan iid source n
    for_ mmsg push

drawCardsIfCanWith
  :: (ReverseQueue m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> source
  -> Int
  -> (CardDraw Message -> CardDraw Message)
  -> m ()
drawCardsIfCanWith iid source n f = do
  when (n > 0) do
    mmsg <- Msg.drawCardsIfCanWith iid source n f
    for_ mmsg push

forcedDrawCards
  :: (ReverseQueue m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator
  -> source
  -> Int
  -> m ()
forcedDrawCards iid source n = when (n > 0) $ push $ Msg.drawCards (asId iid) source n

focusChaosTokens :: ReverseQueue m => [ChaosToken] -> (Message -> m ()) -> m ()
focusChaosTokens tokens f = do
  push $ FocusChaosTokens tokens
  f UnfocusChaosTokens

focusCards :: (ReverseQueue m, IsCard a) => [a] -> (Message -> m ()) -> m ()
focusCards [] _ = pure ()
focusCards cards f = do
  push $ FocusCards $ toCard <$> cards
  f UnfocusCards

focusCards_ :: (ReverseQueue m, IsCard a) => [a] -> m () -> m ()
focusCards_ [] _ = pure ()
focusCards_ cards f = focusCards cards (\unfocus -> f >> push unfocus)

checkWindows :: ReverseQueue m => [Window] -> m ()
checkWindows = Msg.pushM . Msg.checkWindows

checkAfter :: ReverseQueue m => WindowType -> m ()
checkAfter = Msg.pushM . Msg.checkAfter

checkWhen :: ReverseQueue m => WindowType -> m ()
checkWhen = Msg.pushM . Msg.checkWhen

cancelTokenDraw :: (MonadTrans t, HasQueue Message m) => t m ()
cancelTokenDraw = lift Msg.cancelTokenDraw

search
  :: (Targetable target, Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> target
  -> [(Zone, ZoneReturnStrategy)]
  -> ExtendedCardMatcher
  -> FoundCardsStrategy
  -> m ()
search iid source target zones matcher strategy = Msg.push $ Msg.search iid source target zones matcher strategy

lookAt
  :: (Targetable target, Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> target
  -> [(Zone, ZoneReturnStrategy)]
  -> ExtendedCardMatcher
  -> FoundCardsStrategy
  -> m ()
lookAt iid source target zones matcher strategy = Msg.push $ Msg.lookAt iid source target zones matcher strategy

revealing
  :: (Targetable target, Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> target
  -> Zone
  -> m ()
revealing iid (toSource -> source) (toTarget -> target) zone = Msg.push $ Msg.revealing iid source target zone

shuffleIntoDeck :: (ReverseQueue m, IsDeck deck, Targetable target) => deck -> target -> m ()
shuffleIntoDeck deck target = push $ Msg.shuffleIntoDeck deck target

shuffleCardsIntoDeck
  :: (ReverseQueue m, IsDeck deck, MonoFoldable cards, Element cards ~ card, IsCard card)
  => deck
  -> cards
  -> m ()
shuffleCardsIntoDeck deck cards = push $ Msg.shuffleCardsIntoDeck deck cards

shuffleDeck :: (ReverseQueue m, IsDeck deck) => deck -> m ()
shuffleDeck deck = shuffleCardsIntoDeck deck ([] :: [Card])

reduceCostOf :: (Sourceable source, IsCard card, ReverseQueue m) => source -> card -> Int -> m ()
reduceCostOf source card n = Msg.pushM $ Msg.reduceCostOf source card n

gainResourcesModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m ()
gainResourcesModifier iid source target modifier = Msg.pushM $ Msg.gainResourcesModifier iid source target modifier

onRevealChaosTokenEffect
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => SkillTestId
  -> ChaosTokenMatcher
  -> source
  -> target
  -> QueueT Message m ()
  -> m ()
onRevealChaosTokenEffect sid matchr source target f = do
  msgs <- evalQueueT f
  push $ Msg.onRevealChaosTokenEffect sid matchr source target msgs

failOnReveal
  :: (ReverseQueue m, Sourceable attrs, Targetable attrs)
  => ChaosTokenMatcher
  -> SkillTestId
  -> attrs
  -> m ()
failOnReveal matchr sid attrs = onRevealChaosTokenEffect sid matchr attrs attrs failSkillTest

eventModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
eventModifier source target modifier = Msg.pushM $ Msg.eventModifier source target modifier

eventModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
eventModifiers source target modifiers = Msg.pushM $ Msg.eventModifiers source target modifiers

movementModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> ModifierType
  -> m ()
movementModifier source target modifier = Msg.pushM $ Msg.movementModifier source target modifier

dealAdditionalDamage
  :: (HasQueue Message m, MonadTrans t) => InvestigatorId -> Int -> [Message] -> t m ()
dealAdditionalDamage iid amount additionalMessages = lift $ Msg.dealAdditionalDamage iid amount additionalMessages

dealAdditionalHorror
  :: (HasQueue Message m, MonadTrans t) => InvestigatorId -> Int -> [Message] -> t m ()
dealAdditionalHorror iid amount additionalMessages = lift $ Msg.dealAdditionalHorror iid amount additionalMessages

cancelRevelation :: (ReverseQueue m, Sourceable a, IsCard card) => a -> card -> m ()
cancelRevelation a card = do
  cardResolutionModifier card a (CardIdTarget $ toCardId card) IgnoreRevelation
  push $ CancelRevelation (toSource a)

cancelCardEffects :: (ReverseQueue m, Sourceable a, IsCard card) => a -> card -> m ()
cancelCardEffects a card = do
  cardResolutionModifier card a (CardIdTarget $ toCardId card) IgnoreRevelation
  push $ CancelRevelation (toSource a)
  push $ CancelNext (toSource a) DrawEnemyMessage
  push $ CancelSurge (toSource a)

cardResolutionModifier
  :: (ReverseQueue m, IsCard card, Sourceable source, Targetable target)
  => card
  -> source
  -> target
  -> ModifierType
  -> m ()
cardResolutionModifier card source target modifier = Msg.pushM $ Msg.cardResolutionModifier card source target modifier

cardResolutionModifiers
  :: (ReverseQueue m, IsCard card, Sourceable source, Targetable target)
  => card
  -> source
  -> target
  -> [ModifierType]
  -> m ()
cardResolutionModifiers card source target modifiers = Msg.pushM $ Msg.cardResolutionModifiers card source target modifiers

insteadOf
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m))
  => Message
  -> QueueT Message (t m) a
  -> t m ()
insteadOf msg f = do
  msgs <- evalQueueT f
  lift $ replaceMessageMatching (== msg) (const msgs)

insteadOfMatching
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m))
  => (Message -> Bool)
  -> QueueT Message (t m) a
  -> t m ()
insteadOfMatching pred f = do
  msgs <- evalQueueT f
  lift $ replaceMessageMatching pred (const msgs)

don't :: (MonadTrans t, HasQueue Message m) => Message -> t m ()
don't msg = lift $ popMessageMatching_ (== msg)

fromQueue :: (MonadTrans t, HasQueue Message m) => ([Message] -> r) -> t m r
fromQueue f = lift $ Arkham.Classes.HasQueue.fromQueue f

matchingDon't :: (MonadTrans t, HasQueue Message m) => (Message -> Bool) -> t m ()
matchingDon't f = lift $ popMessageMatching_ f

enemyAttackModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
enemyAttackModifier source target modifier = Msg.pushM $ Msg.enemyAttackModifier source target modifier

enemyAttackModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
enemyAttackModifiers source target modifiers = Msg.pushM $ Msg.enemyAttackModifiers source target modifiers

abilityModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => AbilityRef
  -> source
  -> target
  -> ModifierType
  -> m ()
abilityModifier ab source target modifier = Msg.pushM $ Msg.abilityModifier ab source target modifier

batched :: ReverseQueue m => (BatchId -> QueueT Message m ()) -> m ()
batched f = do
  batchId <- getId
  msgs <- evalQueueT (f batchId)
  push $ Would batchId msgs

payBatchCost :: ReverseQueue m => BatchId -> InvestigatorId -> Cost -> m ()
payBatchCost batchId iid cost = push $ PayAdditionalCost iid batchId cost

withCost :: ReverseQueue m => InvestigatorId -> Cost -> QueueT Message m () -> m ()
withCost iid cost f = batched \batchId -> payBatchCost batchId iid cost >> f

oncePerCampaign :: ReverseQueue m => Text -> m () -> m ()
oncePerCampaign k body = do
  stored @Bool k >>= \case
    Nothing -> do
      push $ SetGlobal CampaignTarget (Aeson.fromText k) (toJSON True)
      body
    Just _ -> pure ()

oncePerAbility
  :: (ReverseQueue m, Sourceable attrs, Targetable attrs) => attrs -> Int -> m () -> m ()
oncePerAbility attrs n f = do
  unlessM (getMetaMaybe False attrs "_oncePerAbility") do
    abilityModifier
      (AbilityRef (toSource attrs) n)
      attrs
      attrs
      (MetaModifier $ object ["_oncePerAbility" .= True])
      >> f

insertAfterMatching :: (MonadTrans t, HasQueue msg m) => [msg] -> (msg -> Bool) -> t m ()
insertAfterMatching msgs p = lift $ Msg.insertAfterMatching msgs p

-- Usage:
--      atEndOfTurn iid do
--        addToHand iid (toCard attrs)
atEndOfTurn
  :: (Sourceable a, HasQueue Message m)
  => a
  -> InvestigatorId
  -> QueueT Message m ()
  -> m ()
atEndOfTurn a iid body = do
  msgs <- evalQueueT body
  push $ CreateEndOfTurnEffect (toSource a) iid msgs

-- Usage:
--      atEndOfRound iid do
--        addToHand iid (toCard attrs)
atEndOfRound
  :: (Sourceable a, HasQueue Message m)
  => a
  -> QueueT Message m ()
  -> m ()
atEndOfRound a body = do
  msgs <- evalQueueT body
  push $ CreateEndOfRoundEffect (toSource a) msgs

afterSkillTest
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m)) => QueueT Message (t m) a -> t m ()
afterSkillTest body = do
  msgs <- evalQueueT body
  insertAfterMatching msgs (== EndSkillTestWindow)

afterSearch
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m)) => QueueT Message (t m) a -> t m ()
afterSearch body = do
  msgs <- evalQueueT body
  insertAfterMatching msgs \case
    FinishedSearch {} -> True
    _ -> False

afterEvade
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m)) => QueueT Message (t m) a -> t m ()
afterEvade body = do
  msgs <- evalQueueT body
  insertAfterMatching msgs \case
    AfterEvadeEnemy {} -> True
    _ -> False

delayIfSkillTest
  :: (HasGame (t m), MonadTrans t, HasQueue Message m, HasQueue Message (t m))
  => QueueT Message (t m) a
  -> t m ()
delayIfSkillTest body = do
  msgs <- evalQueueT body
  delay <- Msg.inSkillTest
  if delay
    then insertAfterMatching msgs (== EndSkillTestWindow)
    else pushAll msgs

costModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
costModifier source target modifier = Msg.pushM $ Msg.costModifier source target modifier

costModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
costModifiers source target modifiers = Msg.pushM $ Msg.costModifiers source target modifiers

placeUnderneath :: (ReverseQueue m, Targetable target) => target -> [Card] -> m ()
placeUnderneath (toTarget -> target) cards = push $ Msg.PlaceUnderneath target cards

gainActions :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
gainActions iid (toSource -> source) n = push $ Msg.GainActions iid source n

takeActionAsIfTurn :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
takeActionAsIfTurn iid (toSource -> source) = do
  gainActions iid source 1
  push $ PlayerWindow iid [] False

nonAttackEnemyDamage :: (ReverseQueue m, Sourceable a) => a -> Int -> EnemyId -> m ()
nonAttackEnemyDamage source damage enemy = do
  whenM (enemy <=~> EnemyCanBeDamagedBySource (toSource source)) do
    push $ Msg.EnemyDamage enemy (nonAttack source damage)

attackEnemyDamage :: (ReverseQueue m, Sourceable a) => a -> Int -> EnemyId -> m ()
attackEnemyDamage source damage enemy = do
  whenM (enemy <=~> EnemyCanBeDamagedBySource (toSource source)) do
    push $ Msg.EnemyDamage enemy (attack source damage)

exile :: (ReverseQueue m, Targetable target) => target -> m ()
exile (toTarget -> target) = push $ Msg.Exile target

failSkillTest :: ReverseQueue m => m ()
failSkillTest = push Msg.FailSkillTest

passSkillTest :: ReverseQueue m => m ()
passSkillTest = push Msg.PassSkillTest

ready :: (ReverseQueue m, Targetable target) => target -> m ()
ready = push . Msg.ready

exhaustThis :: (ReverseQueue m, Targetable target) => target -> m ()
exhaustThis = push . Msg.Exhaust . toTarget

readyThis :: (ReverseQueue m, Targetable target) => target -> m ()
readyThis = push . Msg.Ready . toTarget

uiEffect
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
uiEffect s t m = Msg.pushM $ Msg.uiEffect s t m

healDamage
  :: (ReverseQueue m, Sourceable source, Targetable target) => target -> source -> Int -> m ()
healDamage target source n = push $ Msg.HealDamage (toTarget target) (toSource source) n

healHorror
  :: (ReverseQueue m, Sourceable source, Targetable target) => target -> source -> Int -> m ()
healHorror target source n = push $ Msg.HealHorror (toTarget target) (toSource source) n

discoverAtYourLocation
  :: (ReverseQueue m, Sourceable source) => IsInvestigate -> InvestigatorId -> source -> Int -> m ()
discoverAtYourLocation isInvestigate iid s n = do
  withLocationOf iid \loc -> do
    whenM (getCanDiscoverClues isInvestigate iid loc) do
      push $ Msg.DiscoverClues iid $ Msg.discoverAtYourLocation s n

discoverAtYourLocationAndThen
  :: (ReverseQueue m, Sourceable source)
  => IsInvestigate
  -> InvestigatorId
  -> source
  -> Int
  -> QueueT Message m ()
  -> m ()
discoverAtYourLocationAndThen isInvestigate iid s n andThenMsgs = do
  withLocationOf iid \loc -> do
    whenM (getCanDiscoverClues isInvestigate iid loc) do
      msgs <- evalQueueT andThenMsgs
      push $ Msg.DiscoverClues iid $ (Msg.discoverAtYourLocation s n) {Msg.discoverThen = msgs}

discoverAtMatchingLocation
  :: (ReverseQueue m, Sourceable source)
  => IsInvestigate
  -> InvestigatorId
  -> source
  -> LocationMatcher
  -> Int
  -> m ()
discoverAtMatchingLocation isInvestigate iid s mtchr n = do
  locations <- filterM (getCanDiscoverClues isInvestigate iid) =<< select mtchr
  when (notNull locations) do
    Arkham.Message.Lifted.chooseOrRunOne
      iid
      [targetLabel location [Msg.DiscoverClues iid $ Msg.discover location s n] | location <- locations]

discoverAt
  :: (ReverseQueue m, Sourceable source, AsId a, IdOf a ~ LocationId)
  => IsInvestigate
  -> InvestigatorId
  -> source
  -> a
  -> Int
  -> m ()
discoverAt isInvestigate iid s lid n = do
  canDiscover <- getCanDiscoverClues isInvestigate iid (asId lid)
  Msg.pushWhen canDiscover $ Msg.DiscoverClues iid $ Msg.discover lid s n

doStep :: ReverseQueue m => Int -> Message -> m ()
doStep n msg = push $ Msg.DoStep n msg

do_ :: ReverseQueue m => Message -> m ()
do_ msg = push $ Msg.Do msg

twice :: ReverseQueue m => m () -> m ()
twice = repeated 2

repeated :: ReverseQueue m => Int -> m () -> m ()
repeated n = replicateM_ n

disengageEnemy :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
disengageEnemy iid eid = push $ Msg.DisengageEnemy iid eid

disengageFromAll :: (ReverseQueue m, AsId enemy, IdOf enemy ~ EnemyId) => enemy -> m ()
disengageFromAll enemy = push $ Msg.DisengageEnemyFromAll (asId enemy)

cancelledOrIgnoredCardOrGameEffect :: (ReverseQueue m, Sourceable source) => source -> m ()
cancelledOrIgnoredCardOrGameEffect source = checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect (toSource source)

cancelChaosToken
  :: (ReverseQueue (t m), HasQueue Message m, MonadTrans t, Sourceable source)
  => source
  -> ChaosToken
  -> t m ()
cancelChaosToken source token = do
  lift $ Msg.cancelChaosToken token
  push
    $ CancelEachNext
      (toSource source)
      [CheckWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
  cancelledOrIgnoredCardOrGameEffect source

cancelCardDraw
  :: (Sourceable source, ReverseQueue (t m), HasQueue Message m, MonadTrans t)
  => source
  -> Card
  -> t m ()
cancelCardDraw source card = do
  quietCancelCardDraw card
  cancelledOrIgnoredCardOrGameEffect source

quietCancelCardDraw
  :: (ReverseQueue (t m), HasQueue Message m, MonadTrans t)
  => Card
  -> t m ()
quietCancelCardDraw card = do
  mtarget <- getCardEntityTarget card
  lift $ Msg.removeAllMessagesMatching \case
    Do (InvestigatorDrewEncounterCard _ c) -> c.id == card.id
    InvestigatorDrewEncounterCard _ c -> c.id == card.id
    Do (InvestigatorDrewPlayerCardFrom _ c _) -> c.id == card.id
    InvestigatorDrewPlayerCardFrom _ c _ -> c.id == card.id
    DrewTreachery _ _ c -> c.id == card.id
    DrewPlayerEnemy _ c -> c.id == card.id
    Revelation _ (CardIdSource cid) -> cid == card.id
    When (Revelation _ s) -> Just (sourceToTarget s) == mtarget
    Revelation _ s -> Just (sourceToTarget s) == mtarget
    After (Revelation _ s) -> Just (sourceToTarget s) == mtarget
    AfterRevelation _ tid -> case mtarget of
      Just (TreacheryTarget tid') -> tid == tid'
      _ -> False
    _ -> False
  for_ mtarget $ push . QuietlyRemoveFromGame

cancelAttack :: (ReverseQueue m, Sourceable source) => source -> EnemyAttackDetails -> m ()
cancelAttack source _ = push $ CancelNext (toSource source) AttackMessage

changeAttackDetails :: (ReverseQueue m, AsId a, IdOf a ~ EnemyId) => a -> EnemyAttackDetails -> m ()
changeAttackDetails eid details = push $ ChangeEnemyAttackDetails (asId eid) details

cancelEnemyDefeat
  :: (MonadTrans t, HasQueue Message m, AsId enemy, IdOf enemy ~ EnemyId)
  => enemy
  -> t m ()
cancelEnemyDefeat enemy = lift $ Msg.cancelEnemyDefeat (asId enemy)

moveWithSkillTest :: (MonadTrans t, HasQueue Message m) => (Message -> Bool) -> t m ()
moveWithSkillTest f = lift $ Arkham.Classes.HasQueue.mapQueue \msg -> if f msg then MoveWithSkillTest msg else msg

performActionAction
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Action -> m ()
performActionAction iid source action = do
  let windows' = defaultWindows iid
  let decreaseCost = flip applyAbilityModifiers [ActionCostModifier (-1)]
  actions <- filter (`abilityIs` action) <$> getActionsWith iid windows' decreaseCost
  handCards <- field InvestigatorHand iid
  let actionCards = filter (elem action . cdActions . toCardDef) handCards
  playableCards <- filterM (getIsPlayable iid source (UnpaidCost NoAction) windows') actionCards
  when (notNull actions || notNull playableCards) do
    Arkham.Message.Lifted.chooseOne iid
      $ map ((\f -> f windows' [] []) . AbilityLabel iid) actions
      <> [targetLabel (toCardId item) [PayCardCost iid item windows'] | item <- playableCards]

cancelEndTurn :: (MonadTrans t, HasQueue Message m) => InvestigatorId -> t m ()
cancelEndTurn iid = lift $ Msg.removeAllMessagesMatching \case
  When (EndTurn iid') -> iid == iid'
  EndTurn iid' -> iid == iid'
  After (EndTurn iid') -> iid == iid'
  CheckWindows ws -> any isEndTurnWindow ws
  Do (CheckWindows ws) -> any isEndTurnWindow ws
  _ -> False
 where
  isEndTurnWindow w = case w.kind of
    Window.TurnEnds _ -> True
    _ -> False

obtainCard :: (IsCard a, ReverseQueue m) => a -> m ()
obtainCard = push . ObtainCard . toCardId

removeCardFromGame :: (ReverseQueue m, IsCard card) => card -> m ()
removeCardFromGame card = do
  obtainCard card
  push $ RemovedFromGame (toCard card)

playCardPayingCost :: ReverseQueue m => InvestigatorId -> Card -> m ()
playCardPayingCost iid card = do
  addToHandQuiet iid [card]
  withTimings (Window.PlayCard iid $ Window.CardPlay card False) $ payCardCost iid card

payCardCost :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
payCardCost iid card = push $ Msg.PayCardCost iid (toCard card) (defaultWindows iid)

playCardPayingCostWithWindows :: ReverseQueue m => InvestigatorId -> Card -> [Window] -> m ()
playCardPayingCostWithWindows iid card ws = do
  addToHandQuiet iid [card]
  withTimings (Window.PlayCard iid $ Window.CardPlay card False) $ payCardCostWithWindows iid card ws

payCardCostWithWindows :: ReverseQueue m => InvestigatorId -> Card -> [Window] -> m ()
payCardCostWithWindows iid card ws = push $ Msg.PayCardCost iid card ws

removeFromGame :: (ReverseQueue m, Targetable target) => target -> m ()
removeFromGame = push . Msg.RemoveFromGame . toTarget

automaticallyEvadeEnemy :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
automaticallyEvadeEnemy iid eid = push $ Msg.EnemyEvaded iid eid

placeInBonded :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
placeInBonded iid = push . PlaceInBonded iid . toCard

endYourTurn :: ReverseQueue m => InvestigatorId -> m ()
endYourTurn iid = push $ ChooseEndTurn iid

checkDefeated :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
checkDefeated source target = push $ Msg.checkDefeated source target

changeDrawnBy :: (MonadTrans t, HasQueue Message m) => InvestigatorId -> InvestigatorId -> t m ()
changeDrawnBy drawer newDrawer =
  lift $ replaceAllMessagesMatching
    \case
      Revelation me _ -> me == drawer
      Do (InvestigatorDrewEncounterCard me _) -> me == drawer
      InvestigatorDrawEnemy me _ -> me == drawer
      CheckWindows ws -> any (isDrawCard . Window.windowType) ws
      Do (CheckWindows ws) -> any (isDrawCard . Window.windowType) ws
      _ -> False
    \case
      Revelation _ source' -> [Revelation newDrawer source']
      InvestigatorDrawEnemy _ eid -> [InvestigatorDrawEnemy newDrawer eid]
      Do (InvestigatorDrewEncounterCard _ c) -> [Do (InvestigatorDrewEncounterCard newDrawer c)]
      CheckWindows ws -> [CheckWindows $ map changeWindow ws]
      Do (CheckWindows ws) -> [Do (CheckWindows $ map changeWindow ws)]
      _ -> error "wrong message found"
 where
  isDrawCard = \case
    Window.DrawCard who _ _ -> who == drawer
    _ -> False
  changeWindow = \case
    Window.Window t (Window.DrawCard who c f) batchId | who == drawer -> Window.Window t (Window.DrawCard newDrawer c f) batchId
    other -> other

chaosTokenEffect
  :: (ReverseQueue m, Sourceable source) => source -> ChaosToken -> ModifierType -> m ()
chaosTokenEffect (toSource -> source) token modifier =
  Msg.pushM $ Msg.chaosTokenEffect source token modifier

addCurseTokens :: ReverseQueue m => Maybe InvestigatorId -> Int -> m ()
addCurseTokens mWho n = do
  batchId <- getId
  would <-
    Msg.checkWindows
      [ (Window.mkWhen $ Window.WouldAddChaosTokensToChaosBag mWho $ replicate n #curse)
          { Window.windowBatchId = Just batchId
          }
      ]
  Msg.push $ Would batchId $ would : replicate n (Msg.AddChaosToken #curse)

whenNotAtMax :: HasGame m => CardDef -> Int -> (Int -> m ()) -> m ()
whenNotAtMax def n f = do
  mEffect <-
    selectOne $ EffectWithCardCode "maxef" <> EffectWithTarget (CardCodeTarget $ toCardCode def)
  case mEffect of
    Nothing -> f n
    Just effect -> do
      meta <- field EffectMeta effect
      case meta of
        Just (Msg.EffectInt x) -> if x >= n then pure () else f (n - x)
        _ -> error "Invalid meta"

updateMax :: ReverseQueue m => CardDef -> Int -> Msg.EffectWindow -> m ()
updateMax def n ew = do
  mEffect <-
    selectOne $ EffectWithCardCode "maxef" <> EffectWithTarget (CardCodeTarget $ toCardCode def)
  case mEffect of
    Nothing -> push =<< Msg.createMaxEffect def n ew
    Just effect -> do
      meta <- field EffectMeta effect
      case meta of
        Just (Msg.EffectInt x) -> push $ UpdateEffectMeta effect (Msg.EffectInt $ x + n)
        _ -> error "Invalid meta"

takeControlOfAsset
  :: (ReverseQueue m, AsId asset, IdOf asset ~ AssetId) => InvestigatorId -> asset -> m ()
takeControlOfAsset iid asset = push $ Msg.TakeControlOfAsset iid (asId asset)

takeControlOfSetAsideAsset :: ReverseQueue m => InvestigatorId -> Card -> m ()
takeControlOfSetAsideAsset iid card = push $ Msg.TakeControlOfSetAsideAsset iid card

class Attachable a where
  toAttach :: Targetable target => a -> target -> Message

instance Attachable AssetAttrs where
  toAttach attrs target = AttachAsset (asId attrs) (toTarget target)

attach :: (HasQueue Message m, Attachable a, Targetable target) => a -> target -> m ()
attach a = push . toAttach a

enemyCheckEngagement :: ReverseQueue m => EnemyId -> m ()
enemyCheckEngagement = push . EnemyCheckEngagement

enemyEngageInvestigator :: ReverseQueue m => EnemyId -> InvestigatorId -> m ()
enemyEngageInvestigator eid iid = push $ EnemyEngageInvestigator eid iid

advancedWithOther
  :: (ReverseQueue m, Sourceable source, AsId source, IdOf source ~ ActId) => source -> m ()
advancedWithOther source = push $ AdvanceAct (asId source) (toSource source) #other

initiateEnemyAttack
  :: (Targetable target, Sourceable source, IdOf enemy ~ EnemyId, AsId enemy, ReverseQueue m)
  => enemy
  -> source
  -> target
  -> m ()
initiateEnemyAttack enemy source target = push $ InitiateEnemyAttack $ enemyAttack enemy source target

handleTarget
  :: (ReverseQueue m, Sourceable source, Targetable target) => InvestigatorId -> source -> target -> m ()
handleTarget iid source target = push $ Msg.handleTargetChoice iid source target

spendUses
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> UseType -> Int -> m ()
spendUses source target tType n = push $ SpendUses (toSource source) (toTarget target) tType n

discardCard
  :: ( ReverseQueue m
     , Sourceable source
     , IsCard card
     , AsId investigator
     , IdOf investigator ~ InvestigatorId
     )
  => investigator
  -> source
  -> card
  -> m ()
discardCard investigator source card = push $ DiscardCard (asId investigator) (toSource source) (toCardId card)

forTarget :: (ReverseQueue m, Targetable target) => target -> QueueT Message m () -> m ()
forTarget target f =
  evalQueueT f >>= \case
    [] -> pure ()
    [msg] -> push $ ForTarget (toTarget target) msg
    msgs -> push $ ForTarget (toTarget target) (Run msgs)

searchCollectionForRandom
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> CardMatcher -> m ()
searchCollectionForRandom iid source matcher = do
  push $ SearchCollectionForRandom iid (toSource source) matcher

searchCollectionForRandomBasicWeakness
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> [Trait] -> m ()
searchCollectionForRandomBasicWeakness iid source traits = do
  push
    $ SearchCollectionForRandom iid (toSource source) (BasicWeaknessCard <> mapOneOf withTrait traits)

discardUntilFirst
  :: (ReverseQueue m, Sourceable source, IsDeck deck)
  => InvestigatorId
  -> source
  -> deck
  -> ExtendedCardMatcher
  -> m ()
discardUntilFirst iid source deck matcher = do
  push $ DiscardUntilFirst iid (toSource source) (toDeck deck) matcher

createAssetAt_
  :: (ReverseQueue m, IsCard card) => card -> Placement -> m ()
createAssetAt_ c placement = push =<< Msg.createAssetAt_ (toCard c) placement

createAssetAt
  :: (ReverseQueue m, IsCard card) => card -> Placement -> m AssetId
createAssetAt c placement = do
  (assetId, msg) <- Msg.createAssetAt (toCard c) placement
  push msg
  pure assetId

crossOutRecordSetEntries :: (Recordable a, ReverseQueue m) => CampaignLogKey -> [a] -> m ()
crossOutRecordSetEntries _ [] = pure ()
crossOutRecordSetEntries k xs = push $ Msg.crossOutRecordSetEntries k xs

healAllDamage :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
healAllDamage source target = push $ Msg.HealAllDamage (toTarget target) (toSource source)

placeKey :: (ReverseQueue m, Targetable target) => target -> ArkhamKey -> m ()
placeKey target key = push $ Msg.PlaceKey (toTarget target) key

investigatorDefeated :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
investigatorDefeated source iid = push $ Msg.InvestigatorDefeated (toSource source) iid

shuffleSetAsideIntoEncounterDeck :: (ReverseQueue m, IsCardMatcher matcher) => matcher -> m ()
shuffleSetAsideIntoEncounterDeck matcher = do
  cards <- getSetAsideCardsMatching (toCardMatcher matcher)
  push $ ShuffleCardsIntoDeck Deck.EncounterDeck cards

shuffleSetAsideIntoScenarioDeck
  :: (ReverseQueue m, IsCardMatcher matcher) => ScenarioDeckKey -> matcher -> m ()
shuffleSetAsideIntoScenarioDeck key matcher = do
  cards <- getSetAsideCardsMatching (toCardMatcher matcher)
  push $ ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey key) cards

setScenarioDeck :: ReverseQueue m => ScenarioDeckKey -> [Card] -> m ()
setScenarioDeck key cards = push $ Msg.SetScenarioDeck key cards

chooseUpgradeDeck :: ReverseQueue m => InvestigatorId -> m ()
chooseUpgradeDeck iid = do
  pid <- getPlayer iid
  push $ Ask pid ChooseUpgradeDeck

placeCluesOnLocation
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
placeCluesOnLocation iid source n = push $ InvestigatorPlaceCluesOnLocation iid (toSource source) n

drawCard :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
drawCard iid card = case toCard card of
  EncounterCard ec -> push $ InvestigatorDrewEncounterCard iid ec
  PlayerCard pc -> push $ InvestigatorDrewPlayerCardFrom iid pc Nothing
  VengeanceCard vc -> Arkham.Message.Lifted.drawCard iid vc

resign :: ReverseQueue m => InvestigatorId -> m ()
resign iid = push $ Resign iid

discard :: (IsCard card, ReverseQueue m) => card -> m ()
discard card = push $ DiscardedCard (toCardId card)

lookAtRevealed
  :: (ReverseQueue m, Sourceable source, Targetable target) => InvestigatorId -> source -> target -> m ()
lookAtRevealed iid source target = push $ LookAtRevealed iid (toSource source) (toTarget target)

temporaryModifier
  :: (Targetable target, Sourceable source, HasQueue Message m, MonadRandom m, HasGame m)
  => target
  -> source
  -> ModifierType
  -> QueueT Message m ()
  -> m ()
temporaryModifier target source modType = temporaryModifiers target source [modType]

temporaryModifiers
  :: (Targetable target, Sourceable source, HasQueue Message m, MonadRandom m, HasGame m)
  => target
  -> source
  -> [ModifierType]
  -> QueueT Message m ()
  -> m ()
temporaryModifiers target source modTypes body = do
  effectId <- getRandom
  ems <- Msg.effectModifiers source modTypes
  builder <- Msg.makeEffectBuilder "wmode" (Just ems) source target
  msgs <- evalQueueT body
  pushAll $ CreateEffect builder {effectBuilderEffectId = Just effectId}
    : msgs <> [DisableEffect effectId]

discardAllClues
  :: (ReverseQueue m, Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => source
  -> investigator
  -> m ()
discardAllClues source investigator = push $ InvestigatorDiscardAllClues (toSource source) (asId investigator)

cancelSkillTestEffects :: (Sourceable source, ReverseQueue m) => source -> m ()
cancelSkillTestEffects source = do
  Msg.withSkillTest \sid -> do
    canCancelSkillTestEffects <- Msg.getCanCancelSkillTestEffects
    when canCancelSkillTestEffects do
      skillTestModifier sid source sid CancelEffects
      cancelledOrIgnoredCardOrGameEffect source

resolveChaosTokens
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> [ChaosToken] -> m ()
resolveChaosTokens iid source tokens = do
  pushAll
    $ map UnsealChaosToken tokens
    <> map ObtainChaosToken tokens
    <> [ ReplaceCurrentDraw (toSource source) iid
          $ Choose (toSource source) 1 ResolveChoice [Resolved tokens] [] Nothing
       ]

removeLocation :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> m ()
removeLocation (asId -> lid) = do
  noClues <- lid <=~> LocationWithoutClues
  if noClues
    then maybe (push $ RemoveLocation lid) (\_ -> addToVictory lid) =<< field LocationVictory lid
    else push $ RemoveLocation lid

chooseAndDiscardAsset :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseAndDiscardAsset iid source = push $ ChooseAndDiscardAsset iid (toSource source) AnyAsset

loseActions :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
loseActions iid source n = push $ LoseActions iid (toSource source) n

requestChaosTokens :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
requestChaosTokens iid source n = push $ RequestChaosTokens (toSource source) (Just iid) (Reveal n) SetAside

withTimings :: ReverseQueue m => WindowType -> m () -> m ()
withTimings w body = do
  let (before, atIf, after) = Msg.timings w
  checkWindows [before]
  checkWindows [atIf]
  body
  checkWindows [after]

cancelBatch :: ReverseQueue m => BatchId -> m ()
cancelBatch bId = push $ CancelBatch bId

sendMessage :: (ReverseQueue m, Targetable target) => target -> Message -> m ()
sendMessage target msg = push $ SendMessage (toTarget target) msg

setLocationLabel
  :: (AsId location, IdOf location ~ LocationId, ReverseQueue m) => location -> Text -> m ()
setLocationLabel location lbl = push $ SetLocationLabel (asId location) lbl

removeTreachery
  :: (ReverseQueue m, AsId treachery, IdOf treachery ~ TreacheryId) => treachery -> m ()
removeTreachery treachery = push $ RemoveTreachery (asId treachery)

discardTopOfDeck
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, Sourceable source, ReverseQueue m)
  => investigator
  -> source
  -> Int
  -> m ()
discardTopOfDeck investigator source n =
  push $ DiscardTopOfDeck (asId investigator) n (toSource source) Nothing

discardTopOfDeckAndHandle
  :: ( AsId investigator
     , IdOf investigator ~ InvestigatorId
     , Sourceable source
     , Targetable target
     , ReverseQueue m
     )
  => investigator
  -> source
  -> Int
  -> target
  -> m ()
discardTopOfDeckAndHandle investigator source n target =
  push $ DiscardTopOfDeck (asId investigator) n (toSource source) (Just $ toTarget target)

advanceCurrentAct :: (ReverseQueue m, Sourceable source) => source -> m ()
advanceCurrentAct source = do
  actId <- getCurrentAct
  push $ AdvanceAct actId (toSource source) #other

updateLocation
  :: ( ReverseQueue m
     , Eq a
     , Show a
     , Typeable a
     , ToJSON a
     , FromJSON a
     , Show (Field Location a)
     , ToJSON (Field Location a)
     )
  => LocationId
  -> Field Location a
  -> a
  -> m ()
updateLocation lid fld a = push $ UpdateLocation lid $ Update fld a
