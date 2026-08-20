module Arkham.Agenda.Cards.TheDunwichLegacy.TheEssexCountyExpress.RollingBackwards (rollingBackwards) where

import Arkham.Agenda.CardDefs.TheDunwichLegacy.TheEssexCountyExpress qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Direction
import Arkham.Matcher
import Arkham.Scenarios.TheDunwichLegacy.TheEssexCountyExpress.Helpers

newtype RollingBackwards = RollingBackwards AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rollingBackwards :: AgendaCard RollingBackwards
rollingBackwards = agenda (3, A) RollingBackwards Cards.rollingBackwards (Static 4)

instance RunMessage RollingBackwards where
  runMessage msg a@(RollingBackwards attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lid <- leftmostLocation
      removeLocation lid
      removeLocation =<< selectJust (LocationInDirection RightOf $ LocationWithId lid)
      eachInvestigator $ discardAllClues attrs
      advanceAgendaDeck attrs
      pure a
    _ -> RollingBackwards <$> liftRunMessage msg attrs
