module Arkham.Message.Lifted (module X, module Arkham.Message.Lifted) where

import Arkham.Act.Types (ActAttrs (actDeckId))
import Arkham.Agenda.Types (AgendaAttrs (agendaDeckId))
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Classes.Query
import Arkham.DamageEffect
import Arkham.Deck (IsDeck (..))
import Arkham.EffectMetadata (EffectMetadata)
import Arkham.Enemy.Creation
import Arkham.Fight
import Arkham.Helpers
import Arkham.Helpers.Campaign
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Enemy qualified as Msg
import Arkham.Helpers.Log qualified as Msg
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.Window qualified as Msg
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (story)
import Arkham.Movement
import Arkham.Prelude
import Arkham.Query
import Arkham.ScenarioLogKey
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window (Window)
import Arkham.Zone
import Control.Monad.Trans.Class

class (CardGen m, HasGame m, HasQueue Message m) => ReverseQueue m
instance (CardGen m, MonadIO m, HasGame m) => ReverseQueue (QueueT Message m)

setChaosTokens :: ReverseQueue m => [ChaosTokenFace] -> m ()
setChaosTokens = push . SetChaosTokens

setEncounterDeck :: ReverseQueue m => Deck EncounterCard -> m ()
setEncounterDeck = push . SetEncounterDeck

setAgendaDeck :: ReverseQueue m => [CardDef] -> m ()
setAgendaDeck = genCards >=> push . SetAgendaDeckCards 1

setActDeck :: ReverseQueue m => [CardDef] -> m ()
setActDeck = genCards >=> push . SetActDeckCards 1

placeSetAsideLocation :: ReverseQueue m => CardDef -> m LocationId
placeSetAsideLocation card = do
  (lid, msg) <- Msg.placeSetAsideLocation card
  push msg
  pure lid

placeSetAsideLocation_ :: ReverseQueue m => CardDef -> m ()
placeSetAsideLocation_ = push <=< Msg.placeSetAsideLocation_

placeLocationCard
  :: ReverseQueue m => CardDef -> m LocationId
placeLocationCard def = do
  (lid, placement) <- Msg.placeLocationCard def
  push placement
  pure lid

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

placeRandomLocationGroup
  :: ReverseQueue m => Text -> [Card] -> m ()
placeRandomLocationGroup groupName cards = do
  shuffled <- shuffleM cards
  msgs <- Msg.placeLabeledLocations_ groupName shuffled
  pushAll msgs

placeLabeledLocations_ :: ReverseQueue m => Text -> [Card] -> m ()
placeLabeledLocations_ lbl cards = Msg.pushAllM $ Msg.placeLabeledLocations_ lbl cards

placeLabeledLocations :: ReverseQueue m => Text -> [Card] -> m [LocationId]
placeLabeledLocations lbl cards = Msg.placeLabeledLocations lbl cards >>= \(lids, msgs) -> pushAll msgs $> lids

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

reveal :: ReverseQueue m => LocationId -> m ()
reveal = push . Msg.RevealLocation Nothing

moveAllTo :: (ReverseQueue m, Sourceable source) => source -> LocationId -> m ()
moveAllTo (toSource -> source) lid = push $ MoveAllTo source lid

moveTo :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> LocationId -> m ()
moveTo (toSource -> source) iid lid = push $ MoveTo $ move source iid lid

record :: ReverseQueue m => CampaignLogKey -> m ()
record = push . Record

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

incrementRecordCount :: ReverseQueue m => CampaignLogKey -> Int -> m ()
incrementRecordCount key = push . IncrementRecordCount key

story :: ReverseQueue m => FlavorText -> m ()
story flavor = do
  players <- allPlayers
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
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
gainXp iid (toSource -> source) xp = push $ GainXP iid source xp

allGainXpWithBonus
  :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
allGainXpWithBonus (toSource -> source) xp = pushAll =<< toGainXp source (getXpWithBonus xp)

allGainXp
  :: (ReverseQueue m, Sourceable source) => source -> m ()
allGainXp (toSource -> source) = pushAll =<< toGainXp source getXp

endOfScenario :: ReverseQueue m => m ()
endOfScenario = push $ EndOfGame Nothing

assignHorror
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
assignHorror iid (toSource -> source) horror = push $ Msg.assignHorror iid source horror

assignDamageAndHorror
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> Int -> m ()
assignDamageAndHorror iid (toSource -> source) damage horror = push $ Msg.assignDamageAndHorror iid source damage horror

findAndDrawEncounterCard
  :: (ReverseQueue m, IsCardMatcher a) => InvestigatorId -> a -> m ()
findAndDrawEncounterCard iid matcher = push $ Msg.findAndDrawEncounterCard iid matcher

findEncounterCard
  :: forall cardMatcher target m
   . (ReverseQueue m, Targetable target, IsCardMatcher cardMatcher)
  => InvestigatorId
  -> target
  -> cardMatcher
  -> m ()
findEncounterCard iid target cardMatcher =
  push
    $ Msg.FindEncounterCard
      iid
      (toTarget target)
      [FromEncounterDeck, FromEncounterDiscard]
      (toCardMatcher cardMatcher)

beginSkillTest
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType.SkillType
  -> Int
  -> m ()
beginSkillTest iid source target sType n = push $ Msg.beginSkillTest iid source target sType n

gameOverIf :: ReverseQueue m => Bool -> m ()
gameOverIf t = when t (push GameOver)

kill :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
kill (toSource -> source) = push . InvestigatorKilled source

killRemaining
  :: (Sourceable source, ReverseQueue m) => source -> m [InvestigatorId]
killRemaining (toSource -> source) = do
  remaining <- select UneliminatedInvestigator
  resigned <- select ResignedInvestigator
  for_ remaining $ kill source
  gameOverIf (null resigned)
  pure remaining

addCampaignCardToDeck :: ReverseQueue m => InvestigatorId -> CardDef -> m ()
addCampaignCardToDeck investigator cardDef = push $ Msg.AddCampaignCardToDeck investigator cardDef

addCampaignCardToDeckChoice :: ReverseQueue m => [InvestigatorId] -> CardDef -> m ()
addCampaignCardToDeckChoice choices cardDef = do
  lead <- getLeadPlayer
  push $ Msg.addCampaignCardToDeckChoice lead choices cardDef

forceAddCampaignCardToDeckChoice
  :: ReverseQueue m => [InvestigatorId] -> CardDef -> m ()
forceAddCampaignCardToDeckChoice choices cardDef = do
  lead <- getLeadPlayer
  push $ Msg.forceAddCampaignCardToDeckChoice lead choices cardDef

defeatEnemy :: (ReverseQueue m, Sourceable source) => EnemyId -> InvestigatorId -> source -> m ()
defeatEnemy enemyId investigatorId = Msg.defeatEnemy enemyId investigatorId >=> pushAll

createEnemyAt_
  :: (ReverseQueue m, IsCard card) => card -> LocationId -> m ()
createEnemyAt_ c lid = push =<< Msg.createEnemyAt_ (toCard c) lid Nothing

createEnemyAt
  :: (ReverseQueue m, IsCard card) => card -> LocationId -> m EnemyId
createEnemyAt c lid = do
  (enemyId, msg) <- Msg.createEnemyAt (toCard c) lid Nothing
  push msg
  pure enemyId

createSetAsideEnemy
  :: (ReverseQueue m, IsEnemyCreationMethod creation) => CardDef -> creation -> m ()
createSetAsideEnemy def creation = createSetAsideEnemyWith def creation id

createSetAsideEnemyWith
  :: (ReverseQueue m, IsEnemyCreationMethod creation)
  => CardDef
  -> creation
  -> (EnemyCreation Message -> EnemyCreation Message)
  -> m ()
createSetAsideEnemyWith def creation f = do
  card <- getSetAsideCard def
  msg <- Msg.createEnemy card creation
  push $ toMessage (f msg)

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

setAsideCards :: ReverseQueue m => [CardDef] -> m ()
setAsideCards = genCards >=> push . Msg.SetAsideCards

addChaosToken :: ReverseQueue m => ChaosTokenFace -> m ()
addChaosToken = push . AddChaosToken

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

placeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
placeTokens source lid token n = push $ PlaceTokens (toSource source) (toTarget lid) token n

removeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
removeTokens source lid token n = push $ RemoveTokens (toSource source) (toTarget lid) token n

afterSkillTest :: ReverseQueue m => Message -> m ()
afterSkillTest = Msg.pushAfterSkillTest

drawAnotherChaosToken :: ReverseQueue m => InvestigatorId -> m ()
drawAnotherChaosToken = push . DrawAnotherChaosToken

assignEnemyDamage :: ReverseQueue m => DamageAssignment -> EnemyId -> m ()
assignEnemyDamage assignment = push . Msg.assignEnemyDamage assignment

eachInvestigator :: ReverseQueue m => (InvestigatorId -> m ()) -> m ()
eachInvestigator f = do
  investigators <- getInvestigators
  for_ investigators f

selectEach :: (Query a, ReverseQueue m) => a -> (QueryElement a -> m ()) -> m ()
selectEach matcher f = select matcher >>= traverse_ f

selectForEach :: (Query a, ReverseQueue m) => a -> (QueryElement a -> m b) -> m [b]
selectForEach matcher f = select matcher >>= traverse f

advanceAgendaDeck :: ReverseQueue m => AgendaAttrs -> m ()
advanceAgendaDeck attrs = push $ AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)

advanceActDeck :: ReverseQueue m => ActAttrs -> m ()
advanceActDeck attrs = push $ AdvanceActDeck (actDeckId attrs) (toSource attrs)

shuffleEncounterDiscardBackIn :: ReverseQueue m => m ()
shuffleEncounterDiscardBackIn = push ShuffleEncounterDiscardBackIn

placeDoomOnAgenda :: ReverseQueue m => Int -> m ()
placeDoomOnAgenda n = push $ PlaceDoomOnAgenda n CanNotAdvance

placeDoomOnAgendaAndCheckAdvance :: ReverseQueue m => Int -> m ()
placeDoomOnAgendaAndCheckAdvance n = push $ PlaceDoomOnAgenda n CanAdvance

revertAgenda :: (ReverseQueue m, AsId a, IdOf a ~ AgendaId) => a -> m ()
revertAgenda a = push $ RevertAgenda (asId a)

chooseOrRunOne :: ReverseQueue m => InvestigatorId -> [UI Message] -> m ()
chooseOrRunOne iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOrRunOne player msgs

chooseOne :: ReverseQueue m => InvestigatorId -> [UI Message] -> m ()
chooseOne iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOne player msgs

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
  push $ Msg.chooseAmounts player label total choiceMap target

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
  push
    $ Msg.chooseAmounts player label (MaxAmountTarget maxVal) [(choiceLabel, (minVal, maxVal))] target

chooseN :: ReverseQueue m => InvestigatorId -> Int -> [UI Message] -> m ()
chooseN iid n msgs = do
  player <- getPlayer iid
  push $ Msg.chooseN player n msgs

addToHand :: (IsCard a, ReverseQueue m) => InvestigatorId -> [a] -> m ()
addToHand iid cards = push $ AddToHand iid (map toCard cards)

createCardEffect
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => CardDef
  -> Maybe (EffectMetadata Window Message)
  -> source
  -> target
  -> m ()
createCardEffect def mMeta source target = push $ Msg.createCardEffect def mMeta source target

phaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
phaseModifier source target modifier = push $ Msg.phaseModifier source target modifier

gameModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
gameModifier source target modifier = push $ Msg.gameModifier source target modifier

gameModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
gameModifiers source target = traverse_ (gameModifier source target)

flipOver
  :: (ReverseQueue m, Sourceable a, Targetable a) => InvestigatorId -> a -> m ()
flipOver iid a = push $ Msg.Flip iid (toSource a) (toTarget a)

flipOverBy
  :: (ReverseQueue m, Sourceable source, Targetable target) => InvestigatorId -> source -> target -> m ()
flipOverBy iid source target = push $ Msg.Flip iid (toSource source) (toTarget target)

skillTestModifiers
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> [ModifierType]
  -> m ()
skillTestModifiers (toSource -> source) (toTarget -> target) mods =
  push $ Msg.skillTestModifiers source target mods

skillTestModifier
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> ModifierType
  -> m ()
skillTestModifier (toSource -> source) (toTarget -> target) x =
  push $ Msg.skillTestModifier source target x

searchModifier
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> ModifierType
  -> m ()
searchModifier (toSource -> source) (toTarget -> target) modifier =
  push $ Msg.searchModifier source target modifier

chooseFightEnemy :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseFightEnemy iid = mkChooseFight iid >=> push . toMessage

chooseFightEnemyMatch
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> EnemyMatcher -> m ()
chooseFightEnemyMatch iid source = mkChooseFightMatch iid source >=> push . toMessage

mapQueue :: (MonadTrans t, HasQueue Message m) => (Message -> Message) -> t m ()
mapQueue = lift . Msg.mapQueue

toDiscardBy
  :: (ReverseQueue m, Sourceable source, Targetable target) => InvestigatorId -> source -> target -> m ()
toDiscardBy iid source target = push $ Msg.toDiscardBy iid source target

putCardIntoPlay :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
putCardIntoPlay iid card = push $ Msg.putCardIntoPlay iid card

gainResourcesIfCan :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
gainResourcesIfCan iid source n = do
  mmsg <- Msg.gainResourcesIfCan iid source n
  for_ mmsg push

drawCardsIfCan
  :: (ReverseQueue m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> m ()
drawCardsIfCan iid source n = do
  mmsg <- Msg.drawCardsIfCan iid source n
  for_ mmsg push

focusChaosTokens :: ReverseQueue m => [ChaosToken] -> (Message -> m ()) -> m ()
focusChaosTokens tokens f = do
  push $ FocusChaosTokens tokens
  f UnfocusChaosTokens

focusCards :: ReverseQueue m => [Card] -> (Message -> m ()) -> m ()
focusCards cards f = do
  push $ FocusCards cards
  f UnfocusCards

checkWindows :: ReverseQueue m => [Window] -> m ()
checkWindows = Msg.pushM . Msg.checkWindows

cancelTokenDraw :: (MonadTrans t, HasQueue Message m) => t m ()
cancelTokenDraw = lift Msg.cancelTokenDraw

search
  :: (Targetable target, Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> target
  -> [(Zone, ZoneReturnStrategy)]
  -> CardMatcher
  -> FoundCardsStrategy
  -> m ()
search iid source target zones matcher strategy = Msg.push $ Msg.search iid source target zones matcher strategy

lookAt
  :: (Targetable target, Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> target
  -> [(Zone, ZoneReturnStrategy)]
  -> CardMatcher
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

cardResolutionModifier
  :: (ReverseQueue m, IsCard card, Sourceable source, Targetable target)
  => card
  -> source
  -> target
  -> ModifierType
  -> m ()
cardResolutionModifier card source target modifier = push $ Msg.cardResolutionModifier card source target modifier

cardResolutionModifiers
  :: (ReverseQueue m, IsCard card, Sourceable source, Targetable target)
  => card
  -> source
  -> target
  -> [ModifierType]
  -> m ()
cardResolutionModifiers card source target modifiers = push $ Msg.cardResolutionModifiers card source target modifiers
