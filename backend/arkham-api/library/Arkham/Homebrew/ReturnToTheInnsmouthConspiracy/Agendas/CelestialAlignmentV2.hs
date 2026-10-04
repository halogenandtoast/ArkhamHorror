module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Agendas.CelestialAlignmentV2 (celestialAlignmentV2) where

import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Matcher

newtype CelestialAlignmentV2 = CelestialAlignmentV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celestialAlignmentV2 :: AgendaCard CelestialAlignmentV2
celestialAlignmentV2 = agenda (2, A) CelestialAlignmentV2 Cards.celestialAlignmentV2 (Static 7)

instance HasAbilities CelestialAlignmentV2 where
  getAbilities (CelestialAlignmentV2 a) = [needsAir a 1]

instance RunMessage CelestialAlignmentV2 where
  runMessage msg a@(CelestialAlignmentV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- v2: only Hydra wakes on the back (Dagon already woke under the surface),
      -- and the Mother's presence joins the deck.
      lead <- getLead
      selectForMaybeM (enemyIs Enemies.hydraDeepInSlumber) $ flipOver lead
      shuffleSetAsideIntoEncounterDeck [HBTreacheries.presenceOfTheMother]
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> CelestialAlignmentV2 <$> liftRunMessage msg attrs
