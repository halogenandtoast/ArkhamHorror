module Arkham.Asset.Cards.Lantern2 (
  lantern2,
  Lantern2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Lantern2 = Lantern2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lantern2 :: AssetCard Lantern2
lantern2 = asset Lantern2 Cards.lantern2

instance HasAbilities Lantern2 where
  getAbilities (Lantern2 x) =
    [ investigateAbility x 1 (ActionCost 1) ControlsThis
    , doesNotProvokeAttacksOfOpportunity
        $ restrictedAbility x 2 (ControlsThis <> enemyExists (EnemyAt YourLocation))
        $ actionAbilityWithCost (OrCost [discardCost x, removeCost x])
    ]

instance RunMessage Lantern2 where
  runMessage msg a@(Lantern2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust InvestigatorLocation iid
      skillType <- field LocationInvestigateSkill lid
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifier source lid (ShroudModifier (-1))
        , Investigate iid lid source Nothing skillType False
        ]
      pure a
    InDiscard _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      let source = toAbilitySource attrs 2
      targets <- selectTargets $ enemyAtLocationWith iid
      push $ chooseOne iid [TargetLabel target [Damage target source 1] | target <- targets]
      pure a
    InOutOfPlay (UseThisAbility iid (isSource attrs -> True) 2) -> do
      let source = toAbilitySource attrs 2
      enemies <- selectList $ enemyAtLocationWith iid
      push $ chooseOne iid [targetLabel enemy [EnemyDamage enemy $ nonAttack source 2] | enemy <- enemies]
      pure a
    _ -> Lantern2 <$> runMessage msg attrs
