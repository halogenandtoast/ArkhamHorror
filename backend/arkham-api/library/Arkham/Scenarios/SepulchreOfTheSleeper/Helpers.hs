module Arkham.Scenarios.SepulchreOfTheSleeper.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.GameValue
import Arkham.Helpers.Scenario (countScenarioTokens)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Source
import Arkham.Token qualified as Token
import Arkham.Tracing (Tracing)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "sepulchreOfTheSleeper" a

{- | "Disturbance" measures Cthulhu's awareness of the investigators. Both the
/Beneath the City/ agenda and /Dreamer's Rest/ raise it by placing a resource on the
scenario reference card, so the counter is the scenario's own resource token count
rather than anything held in the meta. The skull token scales with it.
-}
getDisturbance :: (HasGame m, Tracing m) => m Int
getDisturbance = countScenarioTokens Token.Resource

-- | Raises the Disturbance by 1; the scenario owns the token on its reference card.
increaseDisturbance :: ReverseQueue m => m ()
increaseDisturbance = scenarioSpecific "increaseDisturbance" ()

{- | Every /Sigil-Carved Alcove/ finishes its [action] the same way: 1 doom onto the
Artifact whose story it tells, and then "investigators at this location may spend 1
[per_investigator] clues, as a group, to place 1 additional doom" on it. @label@ is
the i18n key naming that Artifact in the offer.

The lookup goes through 'artifactInPlay' so it matches an Artifact on either of its
faces — the Obsidian Claw is a different card code once flipped to (Power).
-}
loadArtifact
  :: (HasI18n, ReverseQueue m, Sourceable source)
  => source -> LocationAttrs -> InvestigatorId -> CardDef -> Text -> m ()
loadArtifact source attrs iid def label =
  selectOne (artifactInPlay def) >>= traverse_ \artifact -> do
    placeDoom source artifact 1
    chooseOneM iid do
      labeled' label
        $ withCost iid (GroupClueCost (PerPlayer 1) (be attrs))
        $ placeDoom source artifact 1
      labeled' "doNotPlaceAdditionalDoom" nothing
