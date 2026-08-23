module Arkham.Agenda.Cards.ChildrenOfBlood.NewHorizons.DiggingDeeperV2 (diggingDeeperV2) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DiggingDeeperV2 = DiggingDeeperV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diggingDeeperV2 :: AgendaCard DiggingDeeperV2
diggingDeeperV2 = agenda (2, A) DiggingDeeperV2 Cards.diggingDeeperV2 (Static 14)

instance RunMessage DiggingDeeperV2 where
  runMessage msg a@(DiggingDeeperV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R4
      pure a
    _ -> DiggingDeeperV2 <$> liftRunMessage msg attrs
