module Arkham.Agenda.Cards.TheDrownedCity.TheDrownedQuarter.CollapsingDome (collapsingDome) where

import Arkham.Agenda.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers

newtype CollapsingDome = CollapsingDome AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collapsingDome :: AgendaCard CollapsingDome
collapsingDome = agenda (2, A) CollapsingDome Cards.collapsingDome (Static 7)

instance HasAbilities CollapsingDome where
  getAbilities (CollapsingDome a) = [strugglesForAir a 1]

instance RunMessage CollapsingDome where
  runMessage msg a@(CollapsingDome attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      noResolution
      pure a
    _ -> CollapsingDome <$> liftRunMessage msg attrs
