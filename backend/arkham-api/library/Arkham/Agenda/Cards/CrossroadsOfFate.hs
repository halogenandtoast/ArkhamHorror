module Arkham.Agenda.Cards.CrossroadsOfFate (crossroadsOfFate) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype CrossroadsOfFate = CrossroadsOfFate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crossroadsOfFate :: AgendaCard CrossroadsOfFate
crossroadsOfFate = agenda (2, A) CrossroadsOfFate Cards.crossroadsOfFate (Static 10)

instance RunMessage CrossroadsOfFate where
  runMessage msg a@(CrossroadsOfFate attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      push R5
      pure a
    _ -> CrossroadsOfFate <$> liftRunMessage msg attrs
