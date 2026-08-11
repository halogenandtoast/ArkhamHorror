module Arkham.Homebrew.DarkMatter.Agendas.EmergencyProcedure (emergencyProcedure) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Matcher
import Arkham.Resolution

newtype EmergencyProcedure = EmergencyProcedure AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyProcedure :: AgendaCard EmergencyProcedure
emergencyProcedure = agenda (1, A) EmergencyProcedure Cards.emergencyProcedure (Static 7)

-- "You cannot resign."
instance HasModifiersFor EmergencyProcedure where
  getModifiersFor (EmergencyProcedure a) =
    modifySelect a Anyone [CannotTakeAction $ IsAction #resign]

{- | 1b "Reactor Overload": "Each investigator is killed. The investigators lose
the campaign. There is no resolution."
-}
instance RunMessage EmergencyProcedure where
  runMessage msg a@(EmergencyProcedure attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> push $ InvestigatorKilled (toSource attrs) iid
      push $ ScenarioResolution NoResolution
      pure a
    _ -> EmergencyProcedure <$> liftRunMessage msg attrs
