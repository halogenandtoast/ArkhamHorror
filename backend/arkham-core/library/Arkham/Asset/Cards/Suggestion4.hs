module Arkham.Asset.Cards.Suggestion4 (
  suggestion4,
  Suggestion4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Suggestion4 = Suggestion4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suggestion4 :: AssetCard Suggestion4
suggestion4 = asset Suggestion4 Cards.suggestion4

instance HasAbilities Suggestion4 where
  getAbilities (Suggestion4 a) =
    [ evadeAbility a 1 (ActionCost 1 <> exhaust a) ControlsThis
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility (EnemyWouldAttack Timing.When You (CancelableEnemyAttack AnyEnemyAttack) AnyEnemy)
        $ assetUseCost a Charge 1
    ]

instance RunMessage Suggestion4 where
  runMessage msg a@(Suggestion4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifier source iid (AddSkillValue #willpower)
        , chooseEvadeEnemy iid source #agility
        ]
      pure a
    PassedThisSkillTestBy _ (isSource attrs -> True) n | n < 2 -> do
      push $ SpendUses (toTarget attrs) Charge 1
      pure a
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      push $ SpendUses (toTarget attrs) Charge 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ CancelNext (toAbilitySource attrs 2) AttackMessage
      pure a
    _ -> Suggestion4 <$> runMessage msg attrs
