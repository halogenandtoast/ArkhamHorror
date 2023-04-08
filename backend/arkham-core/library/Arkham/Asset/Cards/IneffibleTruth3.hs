module Arkham.Asset.Cards.IneffibleTruth3
  ( ineffibleTruth3
  , IneffibleTruth3(..)
  , ineffibleTruth3Effect
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

newtype IneffibleTruth3 = IneffibleTruth3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffibleTruth3 :: AssetCard IneffibleTruth3
ineffibleTruth3 = asset IneffibleTruth3 Cards.ineffibleTruth3

instance HasAbilities IneffibleTruth3 where
  getAbilities (IneffibleTruth3 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Evade)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffibleTruth3 where
  runMessage msg a@(IneffibleTruth3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect
          Cards.ineffibleTruth3
          Nothing
          source
          (InvestigatorTarget iid)
        , createCardEffect
          Cards.ineffibleTruth3
          Nothing
          source
          SkillTestTarget
        , skillTestModifier source iid (SkillModifier SkillWillpower 2)
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> IneffibleTruth3 <$> runMessage msg attrs

newtype IneffibleTruth3Effect = IneffibleTruth3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffibleTruth3Effect :: EffectArgs -> IneffibleTruth3Effect
ineffibleTruth3Effect = cardEffect IneffibleTruth3Effect Cards.ineffibleTruth3

instance RunMessage IneffibleTruth3Effect where
  runMessage msg e@(IneffibleTruth3Effect attrs@EffectAttrs {..}) = case msg of
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
    _ -> IneffibleTruth3Effect <$> runMessage msg attrs
