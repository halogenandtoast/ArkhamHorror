module Arkham.Asset.Cards.MedicalTexts (MedicalTexts (..), medicalTexts) where

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

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetCard MedicalTexts
medicalTexts = asset MedicalTexts Cards.medicalTexts

instance HasAbilities MedicalTexts where
  getAbilities (MedicalTexts a) = [restrictedAbility a 1 ControlsThis #action]

instance RunMessage MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let controllerId = getController attrs
      investigators <- select $ affectsOthers $ colocatedWith controllerId
      chooseOne iid
        $ targetLabels investigators
        $ \iid' -> only $ Msg.beginSkillTest iid (attrs.ability 1) iid' #intellect (Fixed 2)
      pure a
    PassedThisSkillTest who (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just target@(InvestigatorTarget iid) -> do
          whenM (withoutModifier who CannotAffectOtherPlayersWithPlayerEffectsExceptDamage)
            $ pushWhenM (canHaveDamageHealed attrs iid)
            $ HealDamage target (toAbilitySource attrs 1) 1
        _ -> error "invalid target"
      pure a
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (InvestigatorTarget iid) -> push $ Msg.assignDamage iid (toAbilitySource attrs 1) 1
        _ -> error "invalid target"
      pure a
    _ -> MedicalTexts <$> liftRunMessage msg attrs
