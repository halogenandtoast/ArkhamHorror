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
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pushAll
        $ [ skillTestModifiers sid attrs iid [DamageDealt 1]
          , createCardEffect Cards.azureFlame Nothing source sid
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
    RevealChaosToken _ iid token -> do
      whenJustM getSkillTest \st -> do
        let triggers =
              token.face `elem` [ElderSign, PlusOne, Zero] && iid == st.investigator && isTarget st attrs.target
        when triggers do
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [InvestigatorAssignDamage iid effectSource DamageAny 1 0]
            , DisableEffect effectId
            ]
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push (DisableEffect effectId)
      pure e
    _ -> AzureFlameEffect <$> runMessage msg attrs
