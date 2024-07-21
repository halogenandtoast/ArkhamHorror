module Arkham.Asset.Cards.AzureFlame5 (azureFlame5, azureFlame5Effect, AzureFlame5 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype AzureFlame5 = AzureFlame5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame5 :: AssetCard AzureFlame5
azureFlame5 = asset AzureFlame5 Cards.azureFlame5

instance HasAbilities AzureFlame5 where
  getAbilities (AzureFlame5 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ Costs [ActionCost 1, assetUseCost a Charge 1]
    ]

instance RunMessage AzureFlame5 where
  runMessage msg a@(AzureFlame5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pushAll
        $ [ skillTestModifiers sid attrs iid [DamageDealt 2, SkillModifier #willpower 3]
          , createCardEffect Cards.azureFlame5 Nothing source sid
          ]
        <> chooseFight
      pure a
    _ -> AzureFlame5 <$> runMessage msg attrs

newtype AzureFlame5Effect = AzureFlame5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame5Effect :: EffectArgs -> AzureFlame5Effect
azureFlame5Effect = cardEffect AzureFlame5Effect Cards.azureFlame5

instance RunMessage AzureFlame5Effect where
  runMessage msg e@(AzureFlame5Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      whenJustM getSkillTest \st -> do
        let triggers =
              token.face `elem` [ElderSign, PlusOne, Zero] && iid == st.investigator && isTarget st attrs.target
        when triggers do
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [InvestigatorAssignDamage iid effectSource DamageAny 2 0]
            , DisableEffect effectId
            ]
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push $ DisableEffect effectId
      pure e
    _ -> AzureFlame5Effect <$> runMessage msg attrs
