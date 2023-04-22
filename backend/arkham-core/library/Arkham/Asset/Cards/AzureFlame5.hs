module Arkham.Asset.Cards.AzureFlame5
  ( azureFlame5
  , azureFlame5Effect
  , AzureFlame5(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Token
import Arkham.Window qualified as Window

newtype AzureFlame5 = AzureFlame5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame5 :: AssetCard AzureFlame5
azureFlame5 = asset AzureFlame5 Cards.azureFlame5

instance HasAbilities AzureFlame5 where
  getAbilities (AzureFlame5 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill
        (Just Action.Fight)
        SkillWillpower
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage AzureFlame5 where
  runMessage msg a@(AzureFlame5 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers attrs (InvestigatorTarget iid) [DamageDealt 2, SkillModifier SkillWillpower 3]
      , createCardEffect Cards.azureFlame5 Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
      ]
    _ -> AzureFlame5 <$> runMessage msg attrs

newtype AzureFlame5Effect = AzureFlame5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame5Effect :: EffectArgs -> AzureFlame5Effect
azureFlame5Effect = cardEffect AzureFlame5Effect Cards.azureFlame5

instance RunMessage AzureFlame5Effect where
  runMessage msg e@(AzureFlame5Effect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (tokenFace token `elem` [ElderSign, PlusOne, Zero])
        (pushAll
          [ If
            (Window.RevealTokenEffect iid token effectId)
            [InvestigatorAssignDamage iid effectSource DamageAny 2 0]
          , DisableEffect effectId
          ]
        )
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> AzureFlame5Effect <$> runMessage msg attrs
