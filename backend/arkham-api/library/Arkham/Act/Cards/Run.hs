module Arkham.Act.Cards.Run (run) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (Run)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheEssexCountyExpress.Helpers
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype Run = Run ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

run :: ActCard Run
run = act (1, A) Run Cards.run Nothing

instance HasAbilities Run where
  getAbilities (Run x) = [mkAbility x 1 $ forced $ Enters #after You "Engine Car"]

instance RunMessage Run where
  runMessage msg a@(Run attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> scenarioI18n do
      investigators <- select $ InvestigatorAt "Engine Car"
      lead <- getLead
      sid <- getRandom
      let runTest iid kind = beginSkillTest sid iid attrs attrs kind (Fixed 3)
      chooseOrRunOneM lead do
        targets investigators \iid -> do
          chooseOneM iid do
            labeled' "dodge" $ runTest iid #agility
            labeled' "endure" $ runTest iid #combat
      advanceActDeck attrs
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) Initiator {} (SkillSkillTest kind) _ | onSide B attrs -> do
      case kind of
        SkillAgility -> sufferMentalTrauma iid 1
        SkillCombat -> sufferPhysicalTrauma iid 1
        _ -> pure ()
      pure a
    _ -> Run <$> liftRunMessage msg attrs
