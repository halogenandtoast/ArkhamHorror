module Arkham.Message.Lifted (module X, module Arkham.Message.Lifted) where

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Classes.Query
import Arkham.Helpers
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Log qualified as Msg
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (story)
import Arkham.Prelude
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Target

class (CardGen m, HasGame m, HasQueue Message m) => ReverseQueue m
instance (CardGen m, MonadIO m, HasGame m) => ReverseQueue (QueueT Message m)

setEncounterDeck :: ReverseQueue m => Deck EncounterCard -> m ()
setEncounterDeck = push . SetEncounterDeck

setAgendaDeck :: ReverseQueue m => [CardDef] -> m ()
setAgendaDeck = genCards >=> push . SetAgendaDeckCards 1

setActDeck :: ReverseQueue m => [CardDef] -> m ()
setActDeck = genCards >=> push . SetActDeckCards 1

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
  :: ReverseQueue m => InvestigatorId -> CardMatcher -> m ()
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

setAsideCards :: ReverseQueue m => [CardDef] -> m ()
setAsideCards = genCards >=> push . Msg.SetAsideCards

addChaosToken :: ReverseQueue m => ChaosTokenFace -> m ()
addChaosToken = push . AddChaosToken
