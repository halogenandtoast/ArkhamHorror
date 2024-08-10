module Arkham.Asset.Cards.BaseballBat (BaseballBat (..), baseballBat, baseballBatEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, withSkillTest)
import Arkham.Modifier

newtype BaseballBat = BaseballBat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat :: AssetCard BaseballBat
baseballBat = asset BaseballBat Cards.baseballBat

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage BaseballBat where
  runMessage msg a@(BaseballBat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]
      createCardEffect Cards.baseballBat (effectMetaTarget sid) source iid
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> BaseballBat <$> liftRunMessage msg attrs

newtype BaseballBatEffect = BaseballBatEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBatEffect :: EffectArgs -> BaseballBatEffect
baseballBatEffect = cardEffect BaseballBatEffect Cards.baseballBat

instance RunMessage BaseballBatEffect where
  runMessage msg e@(BaseballBatEffect attrs) = runQueueT $ case msg of
    SkillTestEnds sid iid _ | maybe False (isTarget sid) attrs.metaTarget -> do
      when (maybe False (isTarget sid) attrs.metaTarget) $ do
        tokens <- map (.face) <$> getSkillTestRevealedChaosTokens
        case attrs.source of
          AbilitySource (AssetSource assetId) 1 ->
            when (any (`elem` [Skull, AutoFail]) tokens) do
              afterSkillTest $ toDiscardBy iid attrs.source assetId
          AbilitySource (ProxySource (CardIdSource _) (AssetSource assetId)) 1 ->
            when (any (`elem` [Skull, AutoFail]) tokens) do
              afterSkillTest $ toDiscardBy iid attrs.source assetId
          _ -> error "wrong source"
      disableReturn e
    _ -> BaseballBatEffect <$> liftRunMessage msg attrs
