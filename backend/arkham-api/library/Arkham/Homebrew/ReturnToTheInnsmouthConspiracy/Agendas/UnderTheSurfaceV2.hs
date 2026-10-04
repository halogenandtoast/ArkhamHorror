module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Agendas.UnderTheSurfaceV2 (underTheSurfaceV2) where

import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.AgentsOfHydra qualified as Enemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Matcher

newtype UnderTheSurfaceV2 = UnderTheSurfaceV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underTheSurfaceV2 :: AgendaCard UnderTheSurfaceV2
underTheSurfaceV2 = agenda (1, A) UnderTheSurfaceV2 Cards.underTheSurfaceV2 (Static 7)

instance HasAbilities UnderTheSurfaceV2 where
  getAbilities (UnderTheSurfaceV2 a) = [needsAir a 1]

instance RunMessage UnderTheSurfaceV2 where
  runMessage msg a@(UnderTheSurfaceV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleSetAsideIntoEncounterDeck [Enemies.lloigor, Enemies.aquaticAbomination]
      shuffleEncounterDiscardBackIn
      -- v2: Dagon wakes here, and the Father's presence joins the deck.
      lead <- getLead
      selectForMaybeM (enemyIs Enemies.dagonDeepInSlumberIntoTheMaelstrom) $ flipOver lead
      shuffleSetAsideIntoEncounterDeck [HBTreacheries.presenceOfTheFather]
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> UnderTheSurfaceV2 <$> liftRunMessage msg attrs
