module Arkham.Message.Lifted (module X, module Arkham.Message.Lifted) where

import Arkham.Ability.Types
import Arkham.Act.Types (ActAttrs (actDeckId))
import Arkham.Agenda.Types (AgendaAttrs (agendaDeckId))
import Arkham.Calculation
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue hiding (insertAfterMatching)
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Deck (IsDeck (..))
import Arkham.Discover as X (IsInvestigate (..))
import Arkham.Discover qualified as Msg
import Arkham.EffectMetadata (EffectMetadata)
import Arkham.Enemy.Creation
import Arkham.Evade
import Arkham.Fight
import Arkham.Helpers
import Arkham.Helpers.Campaign
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Card (getCardEntityTarget)
import Arkham.Helpers.Enemy qualified as Msg
import Arkham.Helpers.Investigator (getCanDiscoverClues, withLocationOf)
import Arkham.Helpers.Log qualified as Msg
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (IgnoreRevelation, MetaModifier), getMetaMaybe)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.Window qualified as Msg
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (story)
import Arkham.Movement
import Arkham.Phase (Phase)
import Arkham.Prelude hiding (pred)
import Arkham.Query
import Arkham.ScenarioLogKey
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window (Window, WindowType)
import Arkham.Window qualified as Window
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
  -> GameCalculation
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

createEnemyEngagedWithPrey :: ReverseQueue m => Card -> m EnemyId
createEnemyEngagedWithPrey c = do
  creation <- Msg.createEnemy c SpawnEngagedWithPrey
  push $ CreateEnemy creation
  pure $ enemyCreationEnemyId creation

createEnemyEngagedWithPrey_ :: ReverseQueue m => Card -> m ()
createEnemyEngagedWithPrey_ = void . createEnemyEngagedWithPrey

createEnemyAt_
  :: (ReverseQueue m, IsCard card) => card -> LocationId -> m ()
createEnemyAt_ c lid = push =<< Msg.createEnemyAt_ (toCard c) lid Nothing

createEnemyAt
  :: (ReverseQueue m, IsCard card) => card -> LocationId -> m EnemyId
createEnemyAt c lid = do
  (enemyId, msg) <- Msg.createEnemyAt (toCard c) lid Nothing
  push msg
  pure enemyId

createEnemyAtLocationMatching_ :: (ReverseQueue m, IsCard card) => card -> LocationMatcher -> m ()
createEnemyAtLocationMatching_ c = Msg.pushM . Msg.createEnemyAtLocationMatching_ (toCard c)

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

removeDoom
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
removeDoom source target n = push $ RemoveDoom (toSource source) (toTarget target) n

placeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
placeTokens source lid token n = push $ PlaceTokens (toSource source) (toTarget lid) token n

removeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
removeTokens source lid token n = push $ RemoveTokens (toSource source) (toTarget lid) token n

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

selectEach :: (Query a, ReverseQueue m) => a -> (QueryElement a -> m ()) -> m ()
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

continue :: ReverseQueue m => InvestigatorId -> [Message] -> m ()
continue iid msgs = Arkham.Message.Lifted.chooseOne iid [Label "Continue" msgs]

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

chooseSome :: ReverseQueue m => InvestigatorId -> Text -> [UI Message] -> m ()
chooseSome iid done msgs = do
  player <- getPlayer iid
  push $ Msg.chooseSome player done msgs

selectOneToHandle
  :: (HasCallStack, ReverseQueue m, Targetable (QueryElement matcher), Query matcher, Sourceable source)
  => InvestigatorId
  -> source
  -> matcher
  -> m ()
selectOneToHandle iid source matcher =
  select matcher >>= \results -> if notNull results then chooseOneToHandle iid source results else pure ()

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
addToHand iid cards = do
  for_ cards obtainCard
  push $ AddToHand iid (map toCard cards)

returnToHand :: (Targetable a, ReverseQueue m) => InvestigatorId -> a -> m ()
returnToHand iid = push . ReturnToHand iid . toTarget

addToVictory :: (ReverseQueue m, Targetable target) => target -> m ()
addToVictory = push . AddToVictory . toTarget

obtainCard :: (IsCard a, ReverseQueue m) => a -> m ()
obtainCard = push . ObtainCard . toCard

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

nextPhaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m ()
nextPhaseModifier phase source target modifier = push $ Msg.nextPhaseModifier phase source target modifier

endOfPhaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => Phase
  -> source
  -> target
  -> ModifierType
  -> m ()
endOfPhaseModifier phase source target modifier = push $ Msg.endOfPhaseModifier phase source target modifier

roundModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
roundModifier source target modifier = push $ Msg.roundModifier source target modifier

skillTestModifiers
  :: forall target source m
   . (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> [ModifierType]
  -> m ()
skillTestModifiers (toSource -> source) (toTarget -> target) mods =
  push $ Msg.skillTestModifiers source target mods

turnModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m ()
turnModifier iid source target modifier = push $ Msg.turnModifier iid source target modifier

turnModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> [ModifierType]
  -> m ()
turnModifiers iid source target modifiers = push $ Msg.turnModifiers iid source target modifiers

setupModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
setupModifier source target modifier = push $ Msg.setupModifier source target modifier

revelationModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> TreacheryId
  -> ModifierType
  -> m ()
revelationModifier (toSource -> source) (toTarget -> target) tid modifier =
  push $ Msg.revelationModifier source target tid modifier

revelationModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source
  -> target
  -> TreacheryId
  -> [ModifierType]
  -> m ()
revelationModifiers (toSource -> source) (toTarget -> target) tid modifiers =
  push $ Msg.revelationModifiers source target tid modifiers

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

chooseEvadeEnemy :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseEvadeEnemy iid = mkChooseEvade iid >=> push . toMessage

chooseEvadeEnemyMatch
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> EnemyMatcher -> m ()
chooseEvadeEnemyMatch iid source = mkChooseEvadeMatch iid source >=> push . toMessage

mapQueue :: (MonadTrans t, HasQueue Message m) => (Message -> Message) -> t m ()
mapQueue = lift . Msg.mapQueue

quietCancelCardDraw :: (MonadTrans t, HasQueue Message m) => Card -> t m ()
quietCancelCardDraw card = lift $ Msg.removeAllMessagesMatching \case
  Do (InvestigatorDrewEncounterCard _ c) -> c.id == card.id
  _ -> False

toDiscardBy
  :: (ReverseQueue m, Sourceable source, Targetable target) => InvestigatorId -> source -> target -> m ()
toDiscardBy iid source target = push $ Msg.toDiscardBy iid source target

toDiscard
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
toDiscard source target = push $ Msg.toDiscard source target

putCardIntoPlay :: (ReverseQueue m, IsCard card) => InvestigatorId -> card -> m ()
putCardIntoPlay iid card = push $ Msg.putCardIntoPlay iid card

gainResourcesIfCan :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
gainResourcesIfCan iid source n = do
  mmsg <- Msg.gainResourcesIfCan iid source n
  for_ mmsg push

drawEncounterCard :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
drawEncounterCard i source = push $ Msg.drawEncounterCards i source 1

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
focusCards [] _ = pure ()
focusCards cards f = do
  push $ FocusCards cards
  f UnfocusCards

checkWindows :: ReverseQueue m => [Window] -> m ()
checkWindows = Msg.pushM . Msg.checkWindows

checkAfter :: ReverseQueue m => WindowType -> m ()
checkAfter = Msg.pushM . Msg.checkAfter

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

reduceCostOf :: (Sourceable source, IsCard card, ReverseQueue m) => source -> card -> Int -> m ()
reduceCostOf source card n = push $ Msg.reduceCostOf source card n

gainResourcesModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> ModifierType
  -> m ()
gainResourcesModifier iid source target modifier = push $ Msg.gainResourcesModifier iid source target modifier

eventModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
eventModifier source target modifier = push $ Msg.eventModifier source target modifier

eventModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
eventModifiers source target modifiers = push $ Msg.eventModifiers source target modifiers

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

matchingDon't :: (MonadTrans t, HasQueue Message m) => (Message -> Bool) -> t m ()
matchingDon't f = lift $ popMessageMatching_ f

enemyAttackModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
enemyAttackModifier source target modifier = push $ Msg.enemyAttackModifier source target modifier

abilityModifier
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => AbilityRef
  -> source
  -> target
  -> ModifierType
  -> m ()
abilityModifier ab source target modifier = push $ Msg.abilityModifier ab source target modifier

batched :: ReverseQueue m => (BatchId -> QueueT Message m ()) -> m ()
batched f = do
  batchId <- getRandom
  msgs <- evalQueueT (f batchId)
  push $ Would batchId msgs

payBatchCost :: ReverseQueue m => BatchId -> InvestigatorId -> Cost -> m ()
payBatchCost batchId iid cost = push $ PayAdditionalCost iid batchId cost

withCost :: ReverseQueue m => InvestigatorId -> Cost -> QueueT Message m () -> m ()
withCost iid cost f = batched \batchId -> payBatchCost batchId iid cost >> f

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

afterSkillTest
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m)) => QueueT Message (t m) a -> t m ()
afterSkillTest body = do
  msgs <- evalQueueT body
  insertAfterMatching msgs (== EndSkillTestWindow)

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
costModifier source target modifier = push $ Msg.costModifier source target modifier

placeUnderneath :: (ReverseQueue m, Targetable target) => target -> [Card] -> m ()
placeUnderneath (toTarget -> target) cards = push $ Msg.PlaceUnderneath target cards

gainActions :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
gainActions iid (toSource -> source) n = push $ Msg.GainActions iid source n

nonAttackEnemyDamage :: (ReverseQueue m, Sourceable a) => a -> Int -> EnemyId -> m ()
nonAttackEnemyDamage source damage enemy = push $ Msg.EnemyDamage enemy (nonAttack source damage)

attackEnemyDamage :: (ReverseQueue m, Sourceable a) => a -> Int -> EnemyId -> m ()
attackEnemyDamage source damage enemy = push $ Msg.EnemyDamage enemy (attack source damage)

exile :: (ReverseQueue m, Targetable target) => target -> m ()
exile (toTarget -> target) = push $ Msg.Exile target

failSkillTest :: ReverseQueue m => m ()
failSkillTest = push Msg.FailSkillTest

uiEffect
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
uiEffect s t m = push $ Msg.uiEffect s t m

healDamage
  :: (ReverseQueue m, Sourceable source, Targetable target) => target -> source -> Int -> m ()
healDamage target source n = push $ Msg.HealDamage (toTarget target) (toSource source) n

discoverAtYourLocation
  :: (ReverseQueue m, Sourceable source) => IsInvestigate -> InvestigatorId -> source -> Int -> m ()
discoverAtYourLocation isInvestigate iid s n = do
  withLocationOf iid \loc -> do
    whenM (getCanDiscoverClues isInvestigate iid loc) do
      push $ Msg.DiscoverClues iid $ Msg.discoverAtYourLocation s n

doStep :: ReverseQueue m => Int -> Message -> m ()
doStep n msg = push $ Msg.DoStep n msg

disengageEnemy :: ReverseQueue m => InvestigatorId -> EnemyId -> m ()
disengageEnemy iid eid = push $ Msg.DisengageEnemy iid eid

cancelledOrIgnoredCardOrGameEffect :: (ReverseQueue m, Sourceable source) => source -> m ()
cancelledOrIgnoredCardOrGameEffect source = checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect (toSource source)

cancelCardDraw
  :: (Sourceable source, ReverseQueue (t m), HasQueue Message m, MonadTrans t)
  => source
  -> Card
  -> t m ()
cancelCardDraw source card = do
  mtarget <- getCardEntityTarget card
  lift $ Msg.removeAllMessagesMatching \case
    Do (InvestigatorDrewEncounterCard _ c) -> c.id == card.id
    InvestigatorDrewEncounterCard _ c -> c.id == card.id
    Do (InvestigatorDrewPlayerCard _ c) -> c.id == card.id
    InvestigatorDrewPlayerCard _ c -> c.id == card.id
    DrewTreachery _ _ c -> c.id == card.id
    DrewPlayerEnemy _ c -> c.id == card.id
    Revelation _ (PlayerCardSource c) -> c.id == card.id
    When (Revelation _ s) -> Just (sourceToTarget s) == mtarget
    Revelation _ s -> Just (sourceToTarget s) == mtarget
    After (Revelation _ s) -> Just (sourceToTarget s) == mtarget
    AfterRevelation _ tid -> case mtarget of
      Just (TreacheryTarget tid') -> tid == tid'
      _ -> False
    _ -> False
  for_ mtarget $ push . QuietlyRemoveFromGame
  cancelledOrIgnoredCardOrGameEffect source
