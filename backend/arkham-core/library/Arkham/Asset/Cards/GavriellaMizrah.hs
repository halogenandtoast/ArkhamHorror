module Arkham.Asset.Cards.GavriellaMizrah (
  gavriellaMizrah,
  GavriellaMizrah (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype GavriellaMizrah = GavriellaMizrah AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellaMizrah :: AssetCard GavriellaMizrah
gavriellaMizrah =
  allyWith GavriellaMizrah Cards.gavriellaMizrah (4, 1) (isStoryL .~ True)

instance HasModifiersFor GavriellaMizrah where
  getModifiersFor (InvestigatorTarget iid) (GavriellaMizrah a) =
    pure [toModifier a (SkillModifier SkillCombat 1) | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    [ restrictedAbility a 1 (ControlsThis <> CanDiscoverCluesAt YourLocation) $
        ReactionAbility (EnemyAttacksEvenIfCancelled Timing.After You AnyEnemyAttack AnyEnemy) (exhaust a)
    ]

instance RunMessage GavriellaMizrah where
  runMessage msg a@(GavriellaMizrah attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid (toSource attrs) 1 Nothing
      pure a
    _ -> GavriellaMizrah <$> runMessage msg attrs
