module Arkham.Asset.Assets.Suggestion4 (suggestion4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Helpers.Window (getAttackDetails)

newtype Suggestion4 = Suggestion4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suggestion4 :: AssetCard Suggestion4
suggestion4 = asset Suggestion4 Cards.suggestion4

instance HasAbilities Suggestion4 where
  getAbilities (Suggestion4 a) =
    [ evadeAbility a 1 (ActionCost 1 <> exhaust a) ControlsThis
    , restricted a 2 ControlsThis
        $ ReactionAbility (EnemyWouldAttack #when You (CancelableEnemyAttack AnyEnemyAttack) AnyEnemy)
        $ assetUseCost a Charge 1
    ]

instance RunMessage Suggestion4 where
  runMessage msg a@(Suggestion4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      skillTestModifier sid source iid (AddSkillValue #willpower)
      chooseEvadeEnemy sid iid source
      pure a
    PassedThisSkillTestBy _ (isSource attrs -> True) n | n < 2 -> do
      push $ SpendUses (attrs.ability 1) (toTarget attrs) Charge 1
      pure a
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      push $ SpendUses (attrs.ability 1) (toTarget attrs) Charge 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getAttackDetails -> details) _ -> do
      cancelAttack (attrs.ability 2) details
      pure a
    _ -> Suggestion4 <$> liftRunMessage msg attrs
