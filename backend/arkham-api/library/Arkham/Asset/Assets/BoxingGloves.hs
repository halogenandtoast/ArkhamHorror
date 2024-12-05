module Arkham.Asset.Assets.BoxingGloves (boxingGloves, BoxingGloves (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait

newtype BoxingGloves = BoxingGloves AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxingGloves :: AssetCard BoxingGloves
boxingGloves = asset BoxingGloves Cards.boxingGloves

instance HasModifiersFor BoxingGloves where
  getModifiersFor (BoxingGloves a) = controllerGets a [ActionSkillModifier #fight #combat 1]

instance HasAbilities BoxingGloves where
  getAbilities (BoxingGloves a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (EnemyDefeated #after You ByAny AnyEnemy)
        $ exhaust a
    ]

instance RunMessage BoxingGloves where
  runMessage msg a@(BoxingGloves attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push
        $ search iid (attrs.ability 1) iid [fromTopOfDeck 6] (basic $ #event <> withTrait Spirit)
        $ DrawFound iid 1
      pure a
    _ -> BoxingGloves <$> runMessage msg attrs
