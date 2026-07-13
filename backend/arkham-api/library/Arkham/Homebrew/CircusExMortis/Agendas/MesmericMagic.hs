module Arkham.Homebrew.CircusExMortis.Agendas.MesmericMagic (mesmericMagic) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype MesmericMagic = MesmericMagic AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmericMagic :: AgendaCard MesmericMagic
mesmericMagic =
  agenda (3, A) MesmericMagic Cards.mesmericMagic (Static 5)

instance RunMessage MesmericMagic where
  runMessage msg a@(MesmericMagic attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) ->
      campaignI18n $ scope "oneNightOnly" $ scope "mesmericMagic" do
        eachInvestigator \iid -> do
          chooseOneM iid do
            labeled' "physicalTrauma" $ sufferPhysicalTrauma iid 1
            labeled' "mentalTrauma" $ sufferMentalTrauma iid 1
          investigatorDefeated attrs iid
        pure a
    _ -> MesmericMagic <$> liftRunMessage msg attrs
