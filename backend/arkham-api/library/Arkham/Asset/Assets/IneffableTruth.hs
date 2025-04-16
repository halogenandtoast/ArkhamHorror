module Arkham.Asset.Assets.IneffableTruth (ineffableTruth) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Evade
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher

newtype IneffableTruth = IneffableTruth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth :: AssetCard IneffableTruth
ineffableTruth = asset IneffableTruth Cards.ineffableTruth

instance HasAbilities IneffableTruth where
  getAbilities (IneffableTruth a) =
    [restricted a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage IneffableTruth where
  runMessage msg a@(IneffableTruth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#eldersign, #"+1", #"0"]) source sid do
        loseResources iid (attrs.ability 1) 1

      createCardEffect Cards.ineffableTruth Nothing source sid
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ (nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1)
      pure a
    _ -> IneffableTruth <$> liftRunMessage msg attrs
