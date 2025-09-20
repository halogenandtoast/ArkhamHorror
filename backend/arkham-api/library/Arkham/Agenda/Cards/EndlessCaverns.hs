module Arkham.Agenda.Cards.EndlessCaverns (endlessCaverns) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Helpers.Query
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype EndlessCaverns = EndlessCaverns AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessCaverns :: AgendaCard EndlessCaverns
endlessCaverns = agenda (3, A) EndlessCaverns Cards.endlessCaverns (Static 4)

instance RunMessage EndlessCaverns where
  runMessage msg a@(EndlessCaverns attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      placePursuitEnemies
      lead <- getLead
      iids <- getInvestigators
      chooseOneM lead do
        scenarioI18n $ questionLabeled' "endlessCaverns.scout"
        targets iids $ handleTarget lead attrs
      advanceAgendaDeck attrs
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid) -> do
      hasRope <- getHasSupply iid Rope
      sid <- getRandom
      unless hasRope do
        chooseOneM iid do
          skillLabeled #combat $ beginSkillTest sid iid attrs attrs #combat (Fixed 5)
          skillLabeled #agility $ beginSkillTest sid iid attrs attrs #agility (Fixed 5)
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      sufferPhysicalTrauma iid 1
      pure a
    _ -> EndlessCaverns <$> liftRunMessage msg attrs
