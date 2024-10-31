module Arkham.Asset.Assets.IneffableTruth (ineffableTruth, IneffableTruth (..), ineffableTruthEffect) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Evade
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype IneffableTruth = IneffableTruth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth :: AssetCard IneffableTruth
ineffableTruth = asset IneffableTruth Cards.ineffableTruth

instance HasAbilities IneffableTruth where
  getAbilities (IneffableTruth a) =
    [restrictedAbility a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage IneffableTruth where
  runMessage msg a@(IneffableTruth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#eldersign, #"+1", #"0"]) attrs attrs do
        loseResources iid (attrs.ability 1) 1

      createCardEffect Cards.ineffableTruth Nothing source sid
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    _ -> IneffableTruth <$> liftRunMessage msg attrs

newtype IneffableTruthEffect = IneffableTruthEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruthEffect :: EffectArgs -> IneffableTruthEffect
ineffableTruthEffect = cardEffect IneffableTruthEffect Cards.ineffableTruth

instance RunMessage IneffableTruthEffect where
  runMessage msg e@(IneffableTruthEffect attrs) = runQueueT $ case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      disableReturn e
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _ -> do
      withSkillTest \sid -> do
        when (isTarget sid attrs.target) do
          nonAttackEnemyDamage iid 1 eid
          disable attrs
      pure e
    _ -> IneffableTruthEffect <$> liftRunMessage msg attrs
