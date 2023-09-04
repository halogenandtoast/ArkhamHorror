module Arkham.Asset.Cards.IneffableTruth5 (
  ineffableTruth5,
  IneffableTruth5 (..),
  ineffableTruth5Effect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype IneffableTruth5 = IneffableTruth5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth5 :: AssetCard IneffableTruth5
ineffableTruth5 = asset IneffableTruth5 Cards.ineffableTruth5

instance HasAbilities IneffableTruth5 where
  getAbilities (IneffableTruth5 a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility
          (Just Action.Evade)
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffableTruth5 where
  runMessage msg a@(IneffableTruth5 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect Cards.ineffableTruth5 Nothing source iid
        , createCardEffect Cards.ineffableTruth5 Nothing source SkillTestTarget
        , skillTestModifier source iid (SkillModifier SkillWillpower 3)
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> IneffableTruth5 <$> runMessage msg attrs

newtype IneffableTruth5Effect = IneffableTruth5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth5Effect :: EffectArgs -> IneffableTruth5Effect
ineffableTruth5Effect = cardEffect IneffableTruth5Effect Cards.ineffableTruth5

instance RunMessage IneffableTruth5Effect where
  runMessage msg e@(IneffableTruth5Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (chaosTokenFace token `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token effectId)
              [LoseResources iid (AbilitySource effectSource 1) 2]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds _ _ ->
      e <$ push (DisableEffect effectId)
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget -> do
          pushAll
            [ EnemyDamage eid $ nonAttack (InvestigatorSource iid) 2
            , DisableEffect effectId
            ]
          pure e
    _ -> IneffableTruth5Effect <$> runMessage msg attrs
