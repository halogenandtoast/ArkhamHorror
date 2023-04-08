module Arkham.Asset.Cards.IneffibleTruth
  ( ineffibleTruth
  , IneffibleTruth(..)
  , ineffibleTruthEffect
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher hiding ( MoveAction )
import Arkham.SkillType
import Arkham.Source
import Arkham.Token
import Arkham.Window qualified as Window

newtype IneffibleTruth = IneffibleTruth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffibleTruth :: AssetCard IneffibleTruth
ineffibleTruth = asset IneffibleTruth Cards.ineffibleTruth

instance HasAbilities IneffibleTruth where
  getAbilities (IneffibleTruth a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Evade)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffibleTruth where
  runMessage msg a@(IneffibleTruth attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect
          Cards.ineffibleTruth
          Nothing
          source
          (InvestigatorTarget iid)
        , createCardEffect
          Cards.ineffibleTruth
          Nothing
          source
          SkillTestTarget
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> IneffibleTruth <$> runMessage msg attrs

newtype IneffibleTruthEffect = IneffibleTruthEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffibleTruthEffect :: EffectArgs -> IneffibleTruthEffect
ineffibleTruthEffect = cardEffect IneffibleTruthEffect Cards.ineffibleTruth

instance RunMessage IneffibleTruthEffect where
  runMessage msg e@(IneffibleTruthEffect attrs@EffectAttrs {..}) = case msg of
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
    _ -> IneffibleTruthEffect <$> runMessage msg attrs
