module Arkham.Asset.Cards.IneffableTruth3 (
  ineffableTruth3,
  IneffableTruth3 (..),
  ineffableTruth3Effect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype IneffableTruth3 = IneffableTruth3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ineffableTruth3 :: AssetCard IneffableTruth3
ineffableTruth3 = asset IneffableTruth3 Cards.ineffableTruth3

instance HasAbilities IneffableTruth3 where
  getAbilities (IneffableTruth3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility
          [Action.Evade]
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffableTruth3 where
  runMessage msg a@(IneffableTruth3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect Cards.ineffableTruth3 Nothing source iid
        , createCardEffect Cards.ineffableTruth3 Nothing source SkillTestTarget
        , skillTestModifier source iid (SkillModifier SkillWillpower 2)
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> IneffableTruth3 <$> runMessage msg attrs

newtype IneffableTruth3Effect = IneffableTruth3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ineffableTruth3Effect :: EffectArgs -> IneffableTruth3Effect
ineffableTruth3Effect = cardEffect IneffableTruth3Effect Cards.ineffableTruth3

instance RunMessage IneffableTruth3Effect where
  runMessage msg e@(IneffableTruth3Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (chaosTokenFace token `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [LoseResources iid (AbilitySource effectSource 1) 1]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds _ _ ->
      e <$ push (DisableEffect effectId)
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget -> do
          pushAll
            [ EnemyDamage eid $ nonAttack (InvestigatorSource iid) 1
            , DisableEffect effectId
            ]
          pure e
    _ -> IneffableTruth3Effect <$> runMessage msg attrs
