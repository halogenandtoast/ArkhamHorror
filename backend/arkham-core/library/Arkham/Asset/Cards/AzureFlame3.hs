module Arkham.Asset.Cards.AzureFlame3 (
  azureFlame3,
  azureFlame3Effect,
  AzureFlame3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype AzureFlame3 = AzureFlame3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame3 :: AssetCard AzureFlame3
azureFlame3 = asset AzureFlame3 Cards.azureFlame3

instance HasAbilities AzureFlame3 where
  getAbilities (AzureFlame3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill
          (Just Action.Fight)
          SkillWillpower
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage AzureFlame3 where
  runMessage msg a@(AzureFlame3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifiers attrs (InvestigatorTarget iid) [DamageDealt 1, SkillModifier SkillWillpower 2]
        , createCardEffect Cards.azureFlame3 Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
        ]
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
      when
        (chaosTokenFace token `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [InvestigatorAssignDamage iid effectSource DamageAny 1 0]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> AzureFlame3Effect <$> runMessage msg attrs
