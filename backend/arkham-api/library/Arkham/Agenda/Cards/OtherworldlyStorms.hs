module Arkham.Agenda.Cards.OtherworldlyStorms (otherworldlyStorms) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Helpers.Window (locationLeavingPlay)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Trait (Trait (Central))

newtype OtherworldlyStorms = OtherworldlyStorms AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyStorms :: AgendaCard OtherworldlyStorms
otherworldlyStorms = agenda (1, A) OtherworldlyStorms Cards.otherworldlyStorms (Static 3)

-- TODO (open sky / Summit deck): The static text on side A ("each non-weakness
-- enemy may enter or leave open sky as though it were a location; each location
-- is connected to each adjacent location and to open sky") relies on the
-- "open sky" placeholder cards and the sliding/Summit-deck infrastructure, which
-- has no engine support yet. Implement those connection/movement modifiers via
-- HasModifiersFor once open-sky locations exist.

instance HasAbilities OtherworldlyStorms where
  getAbilities (OtherworldlyStorms a) =
    [ uncancellable
        $ mkAbility a 1
        $ forced
        $ LocationLeavesPlay #when (LocationWithInvestigator Anyone)
    ]

instance RunMessage OtherworldlyStorms where
  runMessage msg a@(OtherworldlyStorms attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (locationLeavingPlay -> lid) _ -> do
      -- [Forced] When an investigator's location would leave play: move that
      -- investigator to any Central location and deal 2 direct damage to them.
      selectEach (InvestigatorAt $ be lid) \iid -> do
        centralLocations <-
          getCanMoveToMatchingLocations iid (attrs.ability 1) (LocationWithTrait Central)
        chooseTargetM iid centralLocations (moveTo (attrs.ability 1) iid)
        directDamage iid (attrs.ability 1) 2
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      stormIntensity <- getStormIntensity
      if stormIntensity >= 6
        then
          -- The storms reach their peak: each surviving investigator is defeated
          -- and suffers 1 physical trauma.
          eachInvestigator \iid -> do
            sufferPhysicalTrauma iid 1
            investigatorDefeated attrs iid
        else do
          scenarioSpecific "increaseStormIntensity" ()
          -- Flip this agenda back to side A (the storm continues to build).
          push $ RevertAgenda attrs.id
      pure a
    _ -> OtherworldlyStorms <$> liftRunMessage msg attrs
