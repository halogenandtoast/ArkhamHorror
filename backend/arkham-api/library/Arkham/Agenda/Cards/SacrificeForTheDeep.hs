module Arkham.Agenda.Cards.SacrificeForTheDeep (SacrificeForTheDeep (..), sacrificeForTheDeep) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers

newtype SacrificeForTheDeep = SacrificeForTheDeep AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacrificeForTheDeep :: AgendaCard SacrificeForTheDeep
sacrificeForTheDeep = agenda (3, A) SacrificeForTheDeep Cards.sacrificeForTheDeep (Static 4)

instance HasAbilities SacrificeForTheDeep where
  getAbilities (SacrificeForTheDeep a) = [needsAir a 1]

instance RunMessage SacrificeForTheDeep where
  runMessage msg a@(SacrificeForTheDeep attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> SacrificeForTheDeep <$> liftRunMessage msg attrs
