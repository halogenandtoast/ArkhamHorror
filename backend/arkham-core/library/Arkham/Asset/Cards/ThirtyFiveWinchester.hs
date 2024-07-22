module Arkham.Asset.Cards.ThirtyFiveWinchester (
  thirtyFiveWinchester,
  thirtyFiveWinchesterEffect,
  ThirtyFiveWinchester (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Prelude

newtype ThirtyFiveWinchester = ThirtyFiveWinchester AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirtyFiveWinchester :: AssetCard ThirtyFiveWinchester
thirtyFiveWinchester = asset ThirtyFiveWinchester Cards.thirtyFiveWinchester

instance HasAbilities ThirtyFiveWinchester where
  getAbilities (ThirtyFiveWinchester a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage ThirtyFiveWinchester where
  runMessage msg a@(ThirtyFiveWinchester attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll
        [ skillTestModifier sid source iid (SkillModifier #combat 2)
        , createCardEffect Cards.thirtyFiveWinchester Nothing source sid
        , chooseFight
        ]
      pure a
    _ -> ThirtyFiveWinchester <$> runMessage msg attrs

newtype ThirtyFiveWinchesterEffect = ThirtyFiveWinchesterEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirtyFiveWinchesterEffect :: EffectArgs -> ThirtyFiveWinchesterEffect
thirtyFiveWinchesterEffect = cardEffect ThirtyFiveWinchesterEffect Cards.thirtyFiveWinchester

instance RunMessage ThirtyFiveWinchesterEffect where
  runMessage msg e@(ThirtyFiveWinchesterEffect attrs) = case msg of
    ResolveChaosToken _ chaosTokenFace iid -> do
      withSkillTest \sid -> do
        when (isTarget sid attrs.target && chaosTokenFace `elem` [PlusOne, Zero, ElderSign]) do
          pushAll
            [ disable attrs
            , skillTestModifier sid attrs.source iid (DamageDealt 2)
            ]
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push $ disable attrs
      pure e
    _ -> ThirtyFiveWinchesterEffect <$> runMessage msg attrs
