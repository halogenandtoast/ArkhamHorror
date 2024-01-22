module Arkham.Message.Lifted (module X, module Arkham.Message.Lifted) where

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Helpers
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (story)
import Arkham.Prelude
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Target

setEncounterDeck :: HasQueue Message m => Deck EncounterCard -> QueueT Message m ()
setEncounterDeck = push . SetEncounterDeck

setAgendaDeck :: HasQueue Message m => QueueT Message m ()
setAgendaDeck = push SetAgendaDeck

setActDeck :: HasQueue Message m => QueueT Message m ()
setActDeck = push SetActDeck

placeLocationCard
  :: (CardGen m, HasGame m, HasQueue Message m) => CardDef -> QueueT Message m LocationId
placeLocationCard def = do
  (lid, placement) <- Msg.placeLocationCard def
  push placement
  pure lid

reveal :: HasQueue Message m => LocationId -> QueueT Message m ()
reveal = push . Msg.RevealLocation Nothing

moveAllTo :: (HasQueue Message m, Sourceable source) => source -> LocationId -> QueueT Message m ()
moveAllTo (toSource -> source) lid = push $ MoveAllTo source lid

record :: HasQueue Message m => CampaignLogKey -> QueueT Message m ()
record = push . Record

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
