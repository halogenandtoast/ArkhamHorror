module Arkham.Agenda.Cards.BloodMoonCircusExMortis (bloodMoonCircusExMortis) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.CircusExMortis.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype BloodMoonCircusExMortis = BloodMoonCircusExMortis AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodMoonCircusExMortis :: AgendaCard BloodMoonCircusExMortis
bloodMoonCircusExMortis =
  agenda (2, A) BloodMoonCircusExMortis Cards.bloodMoonCircusExMortis (Static 9)

-- Front: "Adjacent copies of Moonlit Forest are connected to each other."
-- (Realized by the Moonlit Forest locations' connectsToAdjacent placement.)

instance RunMessage BloodMoonCircusExMortis where
  runMessage msg a@(BloodMoonCircusExMortis attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) ->
      campaignI18n $ scope "thePrimrosePath" $ scope "bloodMoon" do
        eachInvestigator \iid -> do
          chooseOneM iid do
            labeled' "physicalTrauma" $ sufferPhysicalTrauma iid 1
            labeled' "mentalTrauma" $ sufferMentalTrauma iid 1
          investigatorDefeated attrs iid
        pure a
    _ -> BloodMoonCircusExMortis <$> liftRunMessage msg attrs
