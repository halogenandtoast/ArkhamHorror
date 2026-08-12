module Arkham.Homebrew.DarkMatter.Agendas.EmergencyProcedure (emergencyProcedure) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Matcher

newtype EmergencyProcedure = EmergencyProcedure AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyProcedure :: AgendaCard EmergencyProcedure
emergencyProcedure = agenda (1, A) EmergencyProcedure Cards.emergencyProcedure (Static 7)

instance HasModifiersFor EmergencyProcedure where
  getModifiersFor (EmergencyProcedure a) =
    modifySelect a Anyone [CannotTakeAction $ IsAction #resign]

instance RunMessage EmergencyProcedure where
  runMessage msg a@(EmergencyProcedure attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator $ kill attrs
      gameOver
      pure a
    _ -> EmergencyProcedure <$> liftRunMessage msg attrs
