module Arkham.Asset.Assets.TrustyBullwhip (trustyBullwhip, TrustyBullwhip (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
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
        getSkillTestTarget >>= \case
          Just (EnemyTarget eid) -> do
            canEvade <- eid <=~> EnemyCanBeEvadedBy (attrs.ability 1)
            chooseOneM iid do
              when canEvade do
                labeled "Automatically evade the enemy" do
                  exhaustThis attrs
                  push $ EnemyEvaded iid eid
              labeled "Deal +1 damage for this attack" do
                exhaustThis attrs
                skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
              labeled "Do nothing" nothing
          _ -> error "impossible"
      pure a
    _ -> TrustyBullwhip <$> liftRunMessage msg attrs
