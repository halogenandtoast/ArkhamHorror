module Arkham.Agenda.Cards.CelestialAlignment (CelestialAlignment (..), celestialAlignment) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher

newtype CelestialAlignment = CelestialAlignment AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

celestialAlignment :: AgendaCard CelestialAlignment
celestialAlignment = agenda (2, A) CelestialAlignment Cards.celestialAlignment (Static 8)

instance HasAbilities CelestialAlignment where
  getAbilities (CelestialAlignment a) = [needsAir a 1]

instance RunMessage CelestialAlignment where
  runMessage msg a@(CelestialAlignment attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      selectForMaybeM (IncludeOmnipotent $ enemyIs Enemies.dagonDeepInSlumberIntoTheMaelstrom)
        $ flipOver lead
      selectForMaybeM (IncludeOmnipotent $ enemyIs Enemies.hydraDeepInSlumber) $ flipOver lead
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> CelestialAlignment <$> liftRunMessage msg attrs
