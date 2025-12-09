module Arkham.Asset.Assets.BoxingGloves (boxingGloves) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Strategy
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
    [controlled_ a 1 $ triggered (IfEnemyDefeated #after You ByAny AnyEnemy) (exhaust a)]

instance RunMessage BoxingGloves where
  runMessage msg a@(BoxingGloves attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] (basic $ #event <> withTrait Spirit)
        $ AddFoundToHand iid 1
      pure a
    _ -> BoxingGloves <$> liftRunMessage msg attrs
