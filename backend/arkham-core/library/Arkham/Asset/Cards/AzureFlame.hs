module Arkham.Asset.Cards.AzureFlame (azureFlame, azureFlameEffect, AzureFlame (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype AzureFlame = AzureFlame AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame :: AssetCard AzureFlame
azureFlame = asset AzureFlame Cards.azureFlame

instance HasAbilities AzureFlame where
  getAbilities (AzureFlame a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ Costs [ActionCost 1, assetUseCost a Charge 1]
    ]

instance RunMessage AzureFlame where
  runMessage msg a@(AzureFlame attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight iid source)
      pushAll
        $ [ skillTestModifiers attrs iid [DamageDealt 1]
          , createCardEffect Cards.azureFlame Nothing source iid
          ]
        <> chooseFight
      pure a
    _ -> AzureFlame <$> runMessage msg attrs

newtype AzureFlameEffect = AzureFlameEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlameEffect :: EffectArgs -> AzureFlameEffect
azureFlameEffect = cardEffect AzureFlameEffect Cards.azureFlame

instance RunMessage AzureFlameEffect where
  runMessage msg e@(AzureFlameEffect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when (token.face `elem` [ElderSign, PlusOne, Zero]) do
        pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [InvestigatorAssignDamage iid effectSource DamageAny 1 0]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> AzureFlameEffect <$> runMessage msg attrs
