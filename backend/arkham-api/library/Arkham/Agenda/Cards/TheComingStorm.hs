module Arkham.Agenda.Cards.TheComingStorm (theComingStorm) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers (increaseFloodLevel)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Scenarios.TheDoomOfArkhamPartI.Helpers

newtype TheComingStorm = TheComingStorm AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theComingStorm :: AgendaCard TheComingStorm
theComingStorm = agenda (1, A) TheComingStorm Cards.theComingStorm (Static 14)

instance HasAbilities TheComingStorm where
  getAbilities (TheComingStorm a) =
    guard (onSide A a)
      *> [ scenarioI18n $ withI18nTooltip "theComingStorm.resign" $ mkAbility a 1 resignAction_
         , restricted a 2 (exists $ LocationWithInvestigator LeadInvestigator <> CanHaveFloodLevelIncreased)
             $ forced
             $ PlacedDoomCounter #after AnySource (targetIs a)
         ]

instance RunMessage TheComingStorm where
  runMessage msg a@(TheComingStorm attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- "Increase the flood level of the lead investigator's location." The agenda
      -- enters play already holding one doom per investigator, baked in at
      -- construction, so setup does not flood anything.
      lead <- getLead
      withLocationOf lead increaseFloodLevel
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- "Each investigator who has not been eliminated immediately resigns", which
      -- ends the scenario with no resolution.
      eachInvestigator resign
      pure a
    _ -> TheComingStorm <$> liftRunMessage msg attrs
