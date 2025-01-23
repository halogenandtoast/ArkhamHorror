module Arkham.Asset.Assets.AlchemicalConcoction (alchemicalConcoction) where

import Arkham.Ability
import Arkham.Aspect.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetCard AlchemicalConcoction
alchemicalConcoction = asset AlchemicalConcoction Cards.alchemicalConcoction

instance HasAbilities AlchemicalConcoction where
  getAbilities (AlchemicalConcoction a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      aspect iid source (#intellect `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) eid -> do
      isTheExperiment <- eid <=~> enemyIs Enemies.theExperiment
      when isTheExperiment $ skillTestModifier sid (attrs.ability 1) iid (DamageDealt 6)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      removeFromGame attrs
      pure a
    _ -> AlchemicalConcoction <$> liftRunMessage msg attrs
