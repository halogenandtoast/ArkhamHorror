module Arkham.Asset.Cards.BaseballBat (
  BaseballBat (..),
  baseballBat,
  baseballBatEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner

newtype BaseballBat = BaseballBat AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

baseballBat :: AssetCard BaseballBat
baseballBat = asset BaseballBat Cards.baseballBat

instance HasModifiersFor BaseballBat where
  getModifiersFor (InvestigatorTarget iid) (BaseballBat a) | controlledBy a iid = do
    mAction <- getSkillTestAction
    mSource <- getSkillTestSource
    case (mAction, mSource) of
      (Just Action.Fight, Just (isSource a -> True)) -> pure $ toModifiers a [DamageDealt 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifier source iid (SkillModifier #combat 2)
        , createCardEffect Cards.baseballBat Nothing source iid
        , chooseFightEnemy iid source #combat
        ]
      pure a
    _ -> BaseballBat <$> runMessage msg attrs

newtype BaseballBatEffect = BaseballBatEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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
