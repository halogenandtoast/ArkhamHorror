module Arkham.Agenda.Cards.DrawnIn (DrawnIn (..), drawnIn) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Direction
import Arkham.Matcher
import Arkham.Scenarios.TheEssexCountyExpress.Helpers

newtype DrawnIn = DrawnIn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnIn :: AgendaCard DrawnIn
drawnIn = agenda (4, A) DrawnIn Cards.drawnIn (Static 3)

instance RunMessage DrawnIn where
  runMessage msg a@(DrawnIn attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lid <- leftmostLocation

      removeLocation lid
      removeLocation =<< selectJust (LocationInDirection RightOf $ LocationWithId lid)

      eachInvestigator $ discardAllClues attrs

      advanceAgendaDeck attrs
      pure a
    _ -> DrawnIn <$> liftRunMessage msg attrs
