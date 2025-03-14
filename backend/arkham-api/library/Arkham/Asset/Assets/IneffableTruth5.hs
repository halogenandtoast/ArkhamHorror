module Arkham.Asset.Assets.IneffableTruth5 (ineffableTruth5) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Evade
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype IneffableTruth5 = IneffableTruth5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth5 :: AssetCard IneffableTruth5
ineffableTruth5 = asset IneffableTruth5 Cards.ineffableTruth5

instance HasAbilities IneffableTruth5 where
  getAbilities (IneffableTruth5 a) =
    [restricted a 1 ControlsThis $ evadeAction (assetUseCost a Charge 1)]

instance RunMessage IneffableTruth5 where
  runMessage msg a@(IneffableTruth5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#eldersign, #"+1", #"0"]) source sid do
        loseResources iid (attrs.ability 1) 2
      skillTestModifier sid source iid (SkillModifier #willpower 3)
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ (nonAttackEnemyDamage (attrs.ability 1) 2)
      pure a
    _ -> IneffableTruth5 <$> liftRunMessage msg attrs
