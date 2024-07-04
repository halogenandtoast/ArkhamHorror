module Arkham.Asset.Cards.MedicalTexts2 (MedicalTexts2 (..), medicalTexts2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (getController)
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher

newtype MedicalTexts2 = MedicalTexts2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts2 :: AssetCard MedicalTexts2
medicalTexts2 = asset MedicalTexts2 Cards.medicalTexts2

instance HasAbilities MedicalTexts2 where
  getAbilities (MedicalTexts2 a) = [restrictedAbility a 1 ControlsThis #action]

instance RunMessage MedicalTexts2 where
  runMessage msg a@(MedicalTexts2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let controllerId = getController attrs
      investigators <- select $ affectsOthers $ colocatedWith controllerId
      chooseOne iid
        $ targetLabels investigators
        $ \iid' -> only $ Msg.beginSkillTest iid (attrs.ability 1) iid' #intellect (Fixed 2)
      pure a
    PassedThisSkillTestBy who (isAbilitySource attrs 1 -> True) n -> do
      getSkillTestTarget >>= \case
        Just target@(InvestigatorTarget iid) -> do
          whenM (withoutModifier who CannotAffectOtherPlayersWithPlayerEffectsExceptDamage)
            $ pushWhenM (canHaveDamageHealed attrs iid)
            $ HealDamage target (toAbilitySource attrs 1) (if n >= 2 then 2 else 1)
        _ -> error "invalid target"
      pure a
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (InvestigatorTarget iid) -> do
          chooseOrRunOne iid
            $ [Label "Exhaust Medical Texts" [Exhaust (toTarget attrs)] | attrs.ready]
            <> [Label "Deal 1 damage to that investigator" [Msg.assignDamage iid (toAbilitySource attrs 1) 1]]
        _ -> error "invalid target"
      pure a
    _ -> MedicalTexts2 <$> liftRunMessage msg attrs
