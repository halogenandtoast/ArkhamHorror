module Arkham.Homebrew.CircusExMortis.Agendas.MesmericMagic (mesmericMagic) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype MesmericMagic = MesmericMagic AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmericMagic :: AgendaCard MesmericMagic
mesmericMagic = agenda (3, A) MesmericMagic Cards.mesmericMagic (Static 5)

instance RunMessage MesmericMagic where
  runMessage msg a@(MesmericMagic attrs) = runQueueT $ scenarioI18n "oneNightOnly" $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> scope "mesmericMagic" do
      eachInvestigator \iid -> do
        chooseOneM iid do
          labeled "physicalTrauma" $ sufferPhysicalTrauma iid 1
          labeled "mentalTrauma" $ sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> MesmericMagic <$> liftRunMessage msg attrs
