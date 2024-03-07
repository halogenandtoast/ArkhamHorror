module Arkham.Message.Lifted (module X, module Arkham.Message.Lifted) where

import Arkham.CampaignLogKey
import Arkham.Card
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

setEncounterDeck :: HasQueue Message m => Deck EncounterCard -> QueueT Message m ()
setEncounterDeck = push . SetEncounterDeck

setAgendaDeck :: (HasQueue Message m, CardGen m) => [CardDef] -> QueueT Message m ()
setAgendaDeck = genCards >=> push . SetAgendaDeckCards 1

setActDeck :: (HasQueue Message m, CardGen m) => [CardDef] -> QueueT Message m ()
setActDeck = genCards >=> push . SetActDeckCards 1

placeLocationCard
  :: (CardGen m, HasGame m, HasQueue Message m) => CardDef -> QueueT Message m LocationId
placeLocationCard def = do
  (lid, placement) <- Msg.placeLocationCard def
  push placement
  pure lid

placeRandomLocationGroupCards
  :: (CardGen m, HasGame m, HasQueue Message m) => Text -> [CardDef] -> QueueT Message m ()
placeRandomLocationGroupCards groupName cards = do
  shuffled <- traverse genCard =<< shuffleM cards
  msgs <- Msg.placeLabeledLocations_ groupName shuffled
  pushAll msgs

placeLocationCards
  :: (CardGen m, HasGame m, HasQueue Message m) => [CardDef] -> QueueT Message m ()
placeLocationCards defs = for_ defs placeLocationCard

placeOneLocationCard
  :: (CardGen m, HasGame m, HasQueue Message m) => NonEmpty CardDef -> QueueT Message m LocationId
placeOneLocationCard = sample >=> placeLocationCard

placeLocationCardM
  :: (CardGen m, HasGame m, HasQueue Message m)
  => QueueT Message m CardDef
  -> QueueT Message m LocationId
placeLocationCardM = (>>= placeLocationCard)

reveal :: HasQueue Message m => LocationId -> QueueT Message m ()
reveal = push . Msg.RevealLocation Nothing

moveAllTo :: (HasQueue Message m, Sourceable source) => source -> LocationId -> QueueT Message m ()
moveAllTo (toSource -> source) lid = push $ MoveAllTo source lid

record :: HasQueue Message m => CampaignLogKey -> QueueT Message m ()
record = push . Record

crossOut :: HasQueue Message m => CampaignLogKey -> QueueT Message m ()
crossOut = push . CrossOutRecord

recordSetInsert
  :: (HasQueue Message m, Recordable a, MonoFoldable t, Element t ~ a)
  => CampaignLogKey
  -> t
  -> QueueT Message m ()
recordSetInsert k = push . Msg.recordSetInsert k

story :: (HasQueue Message m, HasGame m) => FlavorText -> QueueT Message m ()
story flavor = do
  players <- allPlayers
  push $ Msg.story players flavor

sufferTrauma :: HasQueue Message m => InvestigatorId -> Int -> Int -> QueueT Message m ()
sufferTrauma iid physical mental = push $ SufferTrauma iid physical mental

sufferMentalTrauma :: HasQueue Message m => InvestigatorId -> Int -> QueueT Message m ()
sufferMentalTrauma iid mental = sufferTrauma iid 0 mental

gainXp
  :: (Sourceable source, HasQueue Message m) => InvestigatorId -> source -> Int -> QueueT Message m ()
gainXp iid (toSource -> source) xp = push $ GainXP iid source xp

allGainXpWithBonus
  :: (Sourceable source, HasQueue Message m, HasGame m) => source -> Int -> QueueT Message m ()
allGainXpWithBonus (toSource -> source) xp = pushAll =<< toGainXp source (getXpWithBonus xp)

allGainXp
  :: (Sourceable source, HasQueue Message m, HasGame m) => source -> QueueT Message m ()
allGainXp (toSource -> source) = pushAll =<< toGainXp source getXp

endOfScenario :: HasQueue Message m => QueueT Message m ()
endOfScenario = push $ EndOfGame Nothing

assignHorror
  :: (Sourceable source, HasQueue Message m) => InvestigatorId -> source -> Int -> QueueT Message m ()
assignHorror iid (toSource -> source) horror = push $ Msg.assignHorror iid source horror

findAndDrawEncounterCard
  :: HasQueue Message m => InvestigatorId -> CardMatcher -> QueueT Message m ()
findAndDrawEncounterCard iid matcher = push $ Msg.findAndDrawEncounterCard iid matcher

beginSkillTest
  :: (HasQueue Message m, Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType.SkillType
  -> Int
  -> QueueT Message m ()
beginSkillTest iid source target sType n = push $ Msg.beginSkillTest iid source target sType n

gameOverIf :: HasQueue Message m => Bool -> QueueT Message m ()
gameOverIf t = if t then push GameOver else pure ()

kill :: (HasQueue Message m, Sourceable source) => source -> InvestigatorId -> QueueT Message m ()
kill (toSource -> source) = push . InvestigatorKilled source

killRemaining
  :: (HasQueue Message m, HasGame m, Sourceable source) => source -> QueueT Message m [InvestigatorId]
killRemaining (toSource -> source) = do
  remaining <- select UneliminatedInvestigator
  resigned <- select ResignedInvestigator
  for_ remaining $ kill source
  gameOverIf (null resigned)
  pure remaining

addCampaignCardToDeckChoice
  :: (HasQueue Message m, HasGame m) => [InvestigatorId] -> CardDef -> QueueT Message m ()
addCampaignCardToDeckChoice choices cardDef = do
  lead <- getLeadPlayer
  push $ Msg.addCampaignCardToDeckChoice lead choices cardDef

createEnemyAt
  :: (HasQueue Message m, IsCard card, MonadRandom m) => card -> LocationId -> QueueT Message m ()
createEnemyAt c lid = push =<< Msg.createEnemyAt_ (toCard c) lid Nothing

setAsideCards :: (CardGen m, HasQueue Message m) => [CardDef] -> QueueT Message m ()
setAsideCards = genCards >=> push . Msg.SetAsideCards
