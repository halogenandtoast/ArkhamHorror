module Arkham.Agenda.Cards.UnderTheSurface (UnderTheSurface (..), underTheSurface) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.Cards qualified as Enemies

newtype UnderTheSurface = UnderTheSurface AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underTheSurface :: AgendaCard UnderTheSurface
underTheSurface = agenda (1, A) UnderTheSurface Cards.underTheSurface (Static 6)

instance HasAbilities UnderTheSurface where
  getAbilities (UnderTheSurface a) = [needsAir a 1]

instance RunMessage UnderTheSurface where
  runMessage msg a@(UnderTheSurface attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleSetAsideIntoEncounterDeck [Enemies.lloigor, Enemies.aquaticAbomination]
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> UnderTheSurface <$> liftRunMessage msg attrs
