module Arkham.Asset.Cards.IneffableTruth
  ( ineffableTruth
  , IneffableTruth(..)
  , ineffableTruthEffect
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher hiding ( MoveAction )
import Arkham.SkillType
import Arkham.Token
import Arkham.Window qualified as Window

newtype IneffableTruth = IneffableTruth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth :: AssetCard IneffableTruth
ineffableTruth = asset IneffableTruth Cards.ineffableTruth

instance HasAbilities IneffableTruth where
  getAbilities (IneffableTruth a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Evade)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffableTruth where
  runMessage msg a@(IneffableTruth attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect
          Cards.ineffableTruth
          Nothing
          source
          (InvestigatorTarget iid)
        , createCardEffect
          Cards.ineffableTruth
          Nothing
          source
          SkillTestTarget
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> IneffableTruth <$> runMessage msg attrs

newtype IneffableTruthEffect = IneffableTruthEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruthEffect :: EffectArgs -> IneffableTruthEffect
ineffableTruthEffect = cardEffect IneffableTruthEffect Cards.ineffableTruth

instance RunMessage IneffableTruthEffect where
  runMessage msg e@(IneffableTruthEffect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (tokenFace token `elem` [ElderSign, PlusOne, Zero])
        (pushAll
          [ If
            (Window.RevealTokenEffect iid token effectId)
            [LoseResources iid (AbilitySource effectSource 1) 1]
          , DisableEffect effectId
          ]
        )
      pure e
    SkillTestEnds _ _ ->
      e <$ push (DisableEffect effectId)
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> e <$ pushAll
        [ EnemyDamage eid $ nonAttack (InvestigatorSource iid) 1
        , DisableEffect effectId
        ]
    _ -> IneffableTruthEffect <$> runMessage msg attrs
