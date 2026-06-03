module Arkham.Agenda.Cards.LambsToTheSlaughter (lambsToTheSlaughter) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype LambsToTheSlaughter = LambsToTheSlaughter AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lambsToTheSlaughter :: AgendaCard LambsToTheSlaughter
lambsToTheSlaughter = agenda (1, A) LambsToTheSlaughter Cards.lambsToTheSlaughter (Static 5)

-- TODO (climactic gameplay): the special-agenda front ("You cannot trigger
-- codex entries", the evade-into-victory-display reaction, and the
-- "1 + 1 per investigator Resident cards in the victory display → R3"
-- objective) is added with the full Final Evening scenario. Only the back side,
-- "Back in the Fold", is implemented here.
instance RunMessage LambsToTheSlaughter where
  runMessage msg a@(LambsToTheSlaughter attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Back in the Fold: each investigator resigns.
      eachInvestigator resign
      pure a
    _ -> LambsToTheSlaughter <$> liftRunMessage msg attrs
