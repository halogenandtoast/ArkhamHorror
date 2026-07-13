module Arkham.Agenda.Cards.MesmericMagicCircusExMortis (mesmericMagicCircusExMortis) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.CircusExMortis.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype MesmericMagicCircusExMortis = MesmericMagicCircusExMortis AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmericMagicCircusExMortis :: AgendaCard MesmericMagicCircusExMortis
mesmericMagicCircusExMortis =
  agenda (3, A) MesmericMagicCircusExMortis Cards.mesmericMagicCircusExMortis (Static 5)

instance RunMessage MesmericMagicCircusExMortis where
  runMessage msg a@(MesmericMagicCircusExMortis attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) ->
      campaignI18n $ scope "oneNightOnly" $ scope "mesmericMagic" do
        eachInvestigator \iid -> do
          chooseOneM iid do
            labeled' "physicalTrauma" $ sufferPhysicalTrauma iid 1
            labeled' "mentalTrauma" $ sufferMentalTrauma iid 1
          investigatorDefeated attrs iid
        pure a
    _ -> MesmericMagicCircusExMortis <$> liftRunMessage msg attrs
