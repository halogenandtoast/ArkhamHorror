module Arkham.Agenda.Cards.SacrificeForTheDeep (
  SacrificeForTheDeep (..),
  sacrificeForTheDeep,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype SacrificeForTheDeep = SacrificeForTheDeep AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sacrificeForTheDeep :: AgendaCard SacrificeForTheDeep
sacrificeForTheDeep = agenda (3, A) SacrificeForTheDeep Cards.sacrificeForTheDeep (Static 4)

instance RunMessage SacrificeForTheDeep where
  runMessage msg a@(SacrificeForTheDeep attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> SacrificeForTheDeep <$> liftRunMessage msg attrs
