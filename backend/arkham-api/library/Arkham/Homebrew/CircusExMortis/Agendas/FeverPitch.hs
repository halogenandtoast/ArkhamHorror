module Arkham.Homebrew.CircusExMortis.Agendas.FeverPitch (feverPitch) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype FeverPitch = FeverPitch AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feverPitch :: AgendaCard FeverPitch
feverPitch = agenda (3, A) FeverPitch Cards.feverPitch (Static 6)

instance RunMessage FeverPitch where
  runMessage msg a@(FeverPitch attrs) = runQueueT $ case msg of
    -- resigned investigators are already eliminated, so 'eachInvestigator' skips them
    AdvanceAgenda (isSide B attrs -> True) ->
      scenarioI18n "bacchanalia" $ scope "feverPitch" do
        eachInvestigator \iid -> do
          chooseOneM iid do
            labeled "physicalTrauma" $ sufferPhysicalTrauma iid 1
            labeled "mentalTrauma" $ sufferMentalTrauma iid 1
          investigatorDefeated attrs iid
        pure a
    _ -> FeverPitch <$> liftRunMessage msg attrs
