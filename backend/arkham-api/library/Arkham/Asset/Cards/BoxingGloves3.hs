module Arkham.Asset.Cards.BoxingGloves3 (boxingGloves3, BoxingGloves3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype BoxingGloves3 = BoxingGloves3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxingGloves3 :: AssetCard BoxingGloves3
boxingGloves3 = asset BoxingGloves3 Cards.boxingGloves3

instance HasModifiersFor BoxingGloves3 where
  getModifiersFor (InvestigatorTarget iid) (BoxingGloves3 a) =
    pure
      [ toModifier a $ ActionSkillModifier #fight #combat 2
      | controlledBy a iid
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities BoxingGloves3 where
  getAbilities (BoxingGloves3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (EnemyDefeated #after You ByAny AnyEnemy) (exhaust a)
    ]

instance RunMessage BoxingGloves3 where
  runMessage msg a@(BoxingGloves3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ search iid (attrs.ability 1) iid [fromTopOfDeck 9] (basic $ #event <> withTrait Spirit)
        $ DrawFound iid 1
      pure a
    _ -> BoxingGloves3 <$> runMessage msg attrs
