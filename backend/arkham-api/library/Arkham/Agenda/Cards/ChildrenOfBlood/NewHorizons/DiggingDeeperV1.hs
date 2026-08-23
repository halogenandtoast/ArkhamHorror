module Arkham.Agenda.Cards.ChildrenOfBlood.NewHorizons.DiggingDeeperV1 (diggingDeeperV1) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype DiggingDeeperV1 = DiggingDeeperV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diggingDeeperV1 :: AgendaCard DiggingDeeperV1
diggingDeeperV1 = agenda (2, A) DiggingDeeperV1 Cards.diggingDeeperV1 (Static 12)

instance RunMessage DiggingDeeperV1 where
  runMessage msg a@(DiggingDeeperV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R4
      pure a
    _ -> DiggingDeeperV1 <$> liftRunMessage msg attrs
