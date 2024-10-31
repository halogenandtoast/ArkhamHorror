module Arkham.Asset.Assets.ThirtyFiveWinchester (
  thirtyFiveWinchester,
  thirtyFiveWinchesterEffect,
  ThirtyFiveWinchester (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Taboo

newtype ThirtyFiveWinchester = ThirtyFiveWinchester AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirtyFiveWinchester :: AssetCard ThirtyFiveWinchester
thirtyFiveWinchester = asset ThirtyFiveWinchester Cards.thirtyFiveWinchester

instance HasAbilities ThirtyFiveWinchester where
  getAbilities (ThirtyFiveWinchester a) =
    [restricted a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage ThirtyFiveWinchester where
  runMessage msg a@(ThirtyFiveWinchester attrs) = runQueueT case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #combat 2)
      createCardEffect
        Cards.thirtyFiveWinchester
        (effectInt $ if tabooed TabooList18 attrs then 1 else 0)
        source
        sid
      chooseFightEnemy sid iid source
      pure a
    _ -> ThirtyFiveWinchester <$> liftRunMessage msg attrs

newtype ThirtyFiveWinchesterEffect = ThirtyFiveWinchesterEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirtyFiveWinchesterEffect :: EffectArgs -> ThirtyFiveWinchesterEffect
thirtyFiveWinchesterEffect = cardEffect ThirtyFiveWinchesterEffect Cards.thirtyFiveWinchester

instance RunMessage ThirtyFiveWinchesterEffect where
  runMessage msg e@(ThirtyFiveWinchesterEffect attrs) = runQueueT $ case msg of
    ResolveChaosToken token _chaosTokenFace iid -> do
      withSkillTest \sid -> do
        valid <-
          if maybe False (== 1) attrs.metaInt
            then token <=~> IncludeTokenPool (IncludeSealed $ not_ WithNegativeModifier)
            else
              token <=~> IncludeTokenPool (IncludeSealed $ mapOneOf ChaosTokenFaceIs [PlusOne, Zero, ElderSign])
        when (isTarget sid attrs.target && valid) do
          disable attrs
          skillTestModifier sid attrs.source iid (DamageDealt 2)
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> ThirtyFiveWinchesterEffect <$> liftRunMessage msg attrs
