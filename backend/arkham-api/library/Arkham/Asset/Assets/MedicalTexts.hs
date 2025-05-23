module Arkham.Asset.Assets.MedicalTexts (medicalTexts) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (getController)
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetCard MedicalTexts
medicalTexts = asset MedicalTexts Cards.medicalTexts

instance HasAbilities MedicalTexts where
  getAbilities (MedicalTexts a) = [skillTestAbility $ restricted a 1 ControlsThis #action]

instance RunMessage MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ colocatedWith $ getController attrs
      sid <- getRandom
      chooseTargetM iid investigators \iid' -> do
        beginSkillTest sid iid (attrs.ability 1) iid' #intellect (Fixed 2)
      pure a
    PassedThisSkillTest who (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \t -> case t.investigator of
        Just iid -> do
          whenM (withoutModifier who CannotAffectOtherPlayersWithPlayerEffectsExceptDamage) do
            whenM (canHaveDamageHealed attrs iid) $ healDamage iid (attrs.ability 1) 1
        _ -> error "invalid target"
      pure a
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \t -> case t.investigator of
        Just iid -> assignDamage iid (attrs.ability 1) 1
        _ -> error "invalid target"
      pure a
    _ -> MedicalTexts <$> liftRunMessage msg attrs
