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
import Arkham.Enemy.Creation
import Arkham.Helpers
import Arkham.Helpers.Campaign
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Log qualified as Msg
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (story)
import Arkham.Movement
import Arkham.Prelude
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token

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

placeRandomLocationGroupCards
  :: ReverseQueue m => Text -> [CardDef] -> m ()
placeRandomLocationGroupCards groupName cards = do
  shuffled <- traverse genCard =<< shuffleM cards
  msgs <- Msg.placeLabeledLocations_ groupName shuffled
  pushAll msgs

placeLabeledLocations_ :: ReverseQueue m => Text -> [Card] -> m ()
placeLabeledLocations_ lbl cards = Msg.pushAllM $ Msg.placeLabeledLocations_ lbl cards

placeLabeledLocations :: ReverseQueue m => Text -> [Card] -> m [LocationId]
placeLabeledLocations lbl cards = Msg.placeLabeledLocations lbl cards >>= \(lids, msgs) -> pushAll msgs $> lids

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

crossOut :: ReverseQueue m => CampaignLogKey -> m ()
crossOut = push . CrossOutRecord

recordSetInsert
  :: (Recordable a, MonoFoldable t, Element t ~ a, ReverseQueue m)
  => CampaignLogKey
  -> t
  -> m ()
recordSetInsert k = push . Msg.recordSetInsert k

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

findAndDrawEncounterCard
  :: (ReverseQueue m, IsCardMatcher a) => InvestigatorId -> a -> m ()
findAndDrawEncounterCard iid matcher = push $ Msg.findAndDrawEncounterCard iid matcher

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

addCampaignCardToDeckChoice
  :: ReverseQueue m => [InvestigatorId] -> CardDef -> m ()
addCampaignCardToDeckChoice choices cardDef = do
  lead <- getLeadPlayer
  push $ Msg.addCampaignCardToDeckChoice lead choices cardDef

forceAddCampaignCardToDeckChoice
  :: ReverseQueue m => [InvestigatorId] -> CardDef -> m ()
forceAddCampaignCardToDeckChoice choices cardDef = do
  lead <- getLeadPlayer
  push $ Msg.forceAddCampaignCardToDeckChoice lead choices cardDef

createEnemyAt
  :: (ReverseQueue m, IsCard card) => card -> LocationId -> m ()
createEnemyAt c lid = push =<< Msg.createEnemyAt_ (toCard c) lid Nothing

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

setAsideCards :: ReverseQueue m => [CardDef] -> m ()
setAsideCards = genCards >=> push . Msg.SetAsideCards

addChaosToken :: ReverseQueue m => ChaosTokenFace -> m ()
addChaosToken = push . AddChaosToken

removeCampaignCard :: (HasCardDef a, ReverseQueue m) => a -> m ()
removeCampaignCard (toCardDef -> def) = do
  mOwner <- getOwner def
  for_ mOwner \owner ->
    push $ RemoveCampaignCardFromDeck owner def

placeClues :: (ReverseQueue m, Sourceable source) => source -> LocationId -> Int -> m ()
placeClues source lid n = push $ PlaceClues (toSource source) (toTarget lid) n

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

advanceAgendaDeck :: ReverseQueue m => AgendaAttrs -> m ()
advanceAgendaDeck attrs = push $ AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)

advanceActDeck :: ReverseQueue m => ActAttrs -> m ()
advanceActDeck attrs = push $ AdvanceActDeck (actDeckId attrs) (toSource attrs)

shuffleEncounterDiscardBackIn :: ReverseQueue m => m ()
shuffleEncounterDiscardBackIn = push ShuffleEncounterDiscardBackIn

placeDoomOnAgenda :: ReverseQueue m => Int -> m ()
placeDoomOnAgenda n = push $ PlaceDoomOnAgenda n CanNotAdvance

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

addToHand :: (IsCard a, ReverseQueue m) => InvestigatorId -> [a] -> m ()
addToHand iid cards = push $ AddToHand iid (map toCard cards)
