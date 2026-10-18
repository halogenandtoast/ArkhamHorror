{- | Scenario-local helpers for Scenario III, A World Torn Down.

Two things live here rather than on a card:

* The i18n scope, as every other scenario does.
* The scenario-log keys the scenario's own cards write and read. Agenda 1b
  ("Time Breaking Slowly") hands the /Aid from Afar/ action to whichever agenda
  is current "for the remainder of the game", and /Aid from Afar/ itself may
  only be resolved one way per scenario; both are scenario-scoped memory, so
  they are 'Arkham.ScenarioLogKey.ScenarioLogKey's rather than campaign-log
  keys.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDown.Helpers where

import Arkham.Ability
import Arkham.Agenda.Types (AgendaAttrs)
import Arkham.Classes.HasQueue (push)
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.Helpers qualified as AgesUnwound
import Arkham.I18n
import Arkham.Id
import Arkham.Message (Message (SetCardAside))
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Message.Lifted.Story (resolveStory)
import Arkham.Prelude
import Arkham.ScenarioLogKey (ScenarioLogKey (HomebrewScenarioLogKey))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = AgesUnwound.scenarioI18n "aWorldTornDown" a

{- | Agenda 1b: "For the remainder of the game, the current agenda gains:
'[action]: Call out for assistance ... Draw the set-aside Aid From Afar story
card and resolve its text.'"

Agenda 1a prints that action itself; agendas 2a and 3a offer it only once this
is remembered. Carrying it on the agendas rather than on the physical reminder
card is what makes "the /current/ agenda" true after the deck advances -- a card
placed next to the agenda deck is a 'Arkham.Card.Card', not an entity, so it has
no abilities to offer.
-}
theCurrentAgendaCallsForAid :: ScenarioLogKey
theCurrentAgendaCallsForAid =
  HomebrewScenarioLogKey "agesUnwound.TheCurrentAgendaCallsForAid"

{- | /Aid from Afar/: "Choose one effect you have not yet chosen this scenario."
One key per printed bullet.
-}
aidFromAfarSearchedTheirDecks :: ScenarioLogKey
aidFromAfarSearchedTheirDecks =
  HomebrewScenarioLogKey "agesUnwound.AidFromAfarSearchedTheirDecks"

aidFromAfarFoundAnInscription :: ScenarioLogKey
aidFromAfarFoundAnInscription =
  HomebrewScenarioLogKey "agesUnwound.AidFromAfarFoundAnInscription"

aidFromAfarCalledInGunfire :: ScenarioLogKey
aidFromAfarCalledInGunfire =
  HomebrewScenarioLogKey "agesUnwound.AidFromAfarCalledInGunfire"

-- | The three keys in printed order, for "an effect you have not yet chosen".
aidFromAfarEffects :: [ScenarioLogKey]
aidFromAfarEffects =
  [ aidFromAfarSearchedTheirDecks
  , aidFromAfarFoundAnInscription
  , aidFromAfarCalledInGunfire
  ]

{- | "[action]: Call out for assistance, and hope your benefactor is listening.
Draw the set-aside Aid From Afar story card and resolve its text."

Printed on agenda 1a.
-}
aidFromAfarAbility :: AgendaAttrs -> Int -> Ability
aidFromAfarAbility a n = mkAbility a n actionAbility

{- | The same ability as agendas 2a and 3a wear it: offered only once agenda 1b
has handed it over. 'HasAbilities' is pure, so the flag has to be read as a
'Arkham.Criteria.Criterion' rather than by querying the scenario log.
-}
grantedAidFromAfarAbility :: AgendaAttrs -> Int -> Ability
grantedAidFromAfarAbility a n =
  restricted a n (Remembered theCurrentAgendaCallsForAid) actionAbility

{- | "Draw the set-aside Aid From Afar story card and resolve its text."

The story's own text ends "set this card aside, out of play", and that has to be
done by hand: reading a story takes its card out of the set-aside pool
(@ReadStoryWithPlacement@ in "Arkham.Scenario.Runner"), so without the trailing
'SetCardAside' the second use of the action finds nothing to draw.

'SetCardAside' is queued behind 'resolveStory', and everything the read spawns
-- the prompt, the resolution, the removal of the story entity -- is pushed in
front of it, so the card only returns once the resolution is over.
-}
drawAidFromAfar :: ReverseQueue m => InvestigatorId -> m ()
drawAidFromAfar iid = do
  card <- getSetAsideCard Stories.aidFromAfar
  resolveStory iid card
  push $ SetCardAside card
