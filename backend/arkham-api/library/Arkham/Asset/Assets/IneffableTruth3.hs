module Arkham.Asset.Assets.IneffableTruth3 (ineffableTruth3) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Evade
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype IneffableTruth3 = IneffableTruth3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth3 :: AssetCard IneffableTruth3
ineffableTruth3 = asset IneffableTruth3 Cards.ineffableTruth3

instance HasAbilities IneffableTruth3 where
  getAbilities (IneffableTruth3 a) =
    [restricted a 1 ControlsThis $ evadeAction (assetUseCost a Charge 1)]

instance RunMessage IneffableTruth3 where
  runMessage msg a@(IneffableTruth3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#eldersign, #"+1", #"0"]) source sid do
        loseResources iid (attrs.ability 1) 1
      skillTestModifier sid source iid (SkillModifier #willpower 2)
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ (nonAttackEnemyDamage (attrs.ability 1) 1)
      pure a
    _ -> IneffableTruth3 <$> liftRunMessage msg attrs
