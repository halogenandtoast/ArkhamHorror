module Arkham.Asset.Cards.AzureFlame
  ( azureFlame
  , azureFlameEffect
  , AzureFlame(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Token
import Arkham.Window qualified as Window

newtype AzureFlame = AzureFlame AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame :: AssetCard AzureFlame
azureFlame = asset AzureFlame Cards.azureFlame

instance HasAbilities AzureFlame where
  getAbilities (AzureFlame a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill
        (Just Action.Fight)
        SkillWillpower
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage AzureFlame where
  runMessage msg a@(AzureFlame attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers attrs (InvestigatorTarget iid) [DamageDealt 1]
      , createCardEffect Cards.azureFlame Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
      ]
    _ -> AzureFlame <$> runMessage msg attrs

newtype AzureFlameEffect = AzureFlameEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlameEffect :: EffectArgs -> AzureFlameEffect
azureFlameEffect = cardEffect AzureFlameEffect Cards.azureFlame

instance RunMessage AzureFlameEffect where
  runMessage msg e@(AzureFlameEffect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (tokenFace token `elem` [ElderSign, PlusOne, Zero])
        (pushAll
          [ If
            (Window.RevealTokenEffect iid token effectId)
            [InvestigatorAssignDamage iid effectSource DamageAny 1 0]
          , DisableEffect effectId
          ]
        )
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> AzureFlameEffect <$> runMessage msg attrs
