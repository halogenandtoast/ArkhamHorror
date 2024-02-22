module Arkham.Asset.Cards.BaseballBat (BaseballBat (..), baseballBat, baseballBatEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Prelude

newtype BaseballBat = BaseballBat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat :: AssetCard BaseballBat
baseballBat = asset BaseballBat Cards.baseballBat

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      pushAll
        [ skillTestModifiers source iid [SkillModifier #combat 2, DamageDealt 1]
        , createCardEffect Cards.baseballBat Nothing source iid
        , chooseFightEnemy iid source #combat
        ]
      pure a
    _ -> BaseballBat <$> runMessage msg attrs

newtype BaseballBatEffect = BaseballBatEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBatEffect :: EffectArgs -> BaseballBatEffect
baseballBatEffect = cardEffect BaseballBatEffect Cards.baseballBat

instance RunMessage BaseballBatEffect where
  runMessage msg e@(BaseballBatEffect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      case effectSource of
        AbilitySource (AssetSource assetId) 1 ->
          when (chaosTokenFace token `elem` [Skull, AutoFail])
            $ pushAll [toDiscardBy iid effectSource assetId, disable attrs]
        _ -> error "wrong source"
      pure e
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> BaseballBatEffect <$> runMessage msg attrs
