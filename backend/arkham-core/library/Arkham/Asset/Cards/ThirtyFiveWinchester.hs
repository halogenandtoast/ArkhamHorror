module Arkham.Asset.Cards.ThirtyFiveWinchester (
  thirtyFiveWinchester,
  thirtyFiveWinchesterEffect,
  ThirtyFiveWinchester (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner

newtype ThirtyFiveWinchester = ThirtyFiveWinchester AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

thirtyFiveWinchester :: AssetCard ThirtyFiveWinchester
thirtyFiveWinchester = asset ThirtyFiveWinchester Cards.thirtyFiveWinchester

instance HasAbilities ThirtyFiveWinchester where
  getAbilities (ThirtyFiveWinchester a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage ThirtyFiveWinchester where
  runMessage msg a@(ThirtyFiveWinchester attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #combat 2)
        , createCardEffect Cards.thirtyFiveWinchester Nothing (toAbilitySource attrs 1) iid
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    _ -> ThirtyFiveWinchester <$> runMessage msg attrs

newtype ThirtyFiveWinchesterEffect = ThirtyFiveWinchesterEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

thirtyFiveWinchesterEffect :: EffectArgs -> ThirtyFiveWinchesterEffect
thirtyFiveWinchesterEffect = cardEffect ThirtyFiveWinchesterEffect Cards.thirtyFiveWinchester

instance RunMessage ThirtyFiveWinchesterEffect where
  runMessage msg e@(ThirtyFiveWinchesterEffect attrs) = case msg of
    ResolveChaosToken _ chaosTokenFace _ -> do
      case attrs.target of
        InvestigatorTarget iid ->
          when (chaosTokenFace `elem` [PlusOne, Zero, ElderSign])
            $ pushAll
              [ disable attrs
              , skillTestModifier attrs.source iid (DamageDealt 2)
              ]
        _ -> error "Wrong target type"
      pure e
    SkillTestEnds {} -> do
      push $ disable attrs
      pure e
    _ -> ThirtyFiveWinchesterEffect <$> runMessage msg attrs
