module Arkham.Asset.Cards.AzureFlame3 (azureFlame3, azureFlame3Effect, AzureFlame3 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype AzureFlame3 = AzureFlame3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame3 :: AssetCard AzureFlame3
azureFlame3 = asset AzureFlame3 Cards.azureFlame3

instance HasAbilities AzureFlame3 where
  getAbilities (AzureFlame3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ Costs [ActionCost 1, assetUseCost a Charge 1]
    ]

instance RunMessage AzureFlame3 where
  runMessage msg a@(AzureFlame3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pushAll
        $ [ skillTestModifiers sid attrs iid [DamageDealt 1, SkillModifier #willpower 2]
          , createCardEffect Cards.azureFlame3 Nothing source sid
          ]
        <> chooseFight
      pure a
    _ -> AzureFlame3 <$> runMessage msg attrs

newtype AzureFlame3Effect = AzureFlame3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame3Effect :: EffectArgs -> AzureFlame3Effect
azureFlame3Effect = cardEffect AzureFlame3Effect Cards.azureFlame3

instance RunMessage AzureFlame3Effect where
  runMessage msg e@(AzureFlame3Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      whenJustM getSkillTest \st -> do
        let triggers =
              token.face `elem` [ElderSign, PlusOne, Zero] && iid == st.investigator && isTarget st attrs.target
        when triggers $ do
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token effectId)
                [InvestigatorAssignDamage iid effectSource DamageAny 1 0]
            , DisableEffect effectId
            ]
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push $ DisableEffect effectId
      pure e
    _ -> AzureFlame3Effect <$> runMessage msg attrs
