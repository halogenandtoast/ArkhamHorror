module Arkham.Asset.Assets.TrustyBullwhip (trustyBullwhip) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy, withSkillTest)
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype TrustyBullwhip = TrustyBullwhip AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trustyBullwhip :: AssetCard TrustyBullwhip
trustyBullwhip = asset TrustyBullwhip Cards.trustyBullwhip

instance HasAbilities TrustyBullwhip where
  getAbilities (TrustyBullwhip a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill [#fight] #agility $ ActionCost 1]

instance RunMessage TrustyBullwhip where
  runMessage msg a@(TrustyBullwhip attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      aspect iid source (#agility `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTest \sid -> when attrs.ready do
        mEnemy <- getSkillTestTargetedEnemy
        chooseOneM iid $ withI18n do
          for_ mEnemy \eid -> do
            canEvade <- eid <=~> EnemyCanBeEvadedBy (attrs.ability 1)
            when canEvade do
              card <- fetchCard eid
              cardNameVar card $ labeledValidate' canEvade "automaticallyEvade" do
                exhaustThis attrs
                automaticallyEvadeEnemy iid eid
          countVar 1 $ labeled' "dealAdditionalDamage" do
            exhaustThis attrs
            skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
          labeled' "skip" nothing
      pure a
    _ -> TrustyBullwhip <$> liftRunMessage msg attrs
