module Arkham.Asset.Cards.IneffibleTruth5
  ( ineffibleTruth5
  , IneffibleTruth5(..)
  , ineffibleTruth5Effect
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

newtype IneffibleTruth5 = IneffibleTruth5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffibleTruth5 :: AssetCard IneffibleTruth5
ineffibleTruth5 = asset IneffibleTruth5 Cards.ineffibleTruth5

instance HasAbilities IneffibleTruth5 where
  getAbilities (IneffibleTruth5 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Evade)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffibleTruth5 where
  runMessage msg a@(IneffibleTruth5 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ createCardEffect
          Cards.ineffibleTruth5
          Nothing
          source
          (InvestigatorTarget iid)
        , createCardEffect
          Cards.ineffibleTruth5
          Nothing
          source
          SkillTestTarget
        , skillTestModifier source iid (SkillModifier SkillWillpower 3)
        , ChooseEvadeEnemy iid source Nothing SkillWillpower AnyEnemy False
        ]
      pure a
    _ -> IneffibleTruth5 <$> runMessage msg attrs

newtype IneffibleTruth5Effect = IneffibleTruth5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffibleTruth5Effect :: EffectArgs -> IneffibleTruth5Effect
ineffibleTruth5Effect = cardEffect IneffibleTruth5Effect Cards.ineffibleTruth5

instance RunMessage IneffibleTruth5Effect where
  runMessage msg e@(IneffibleTruth5Effect attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when
        (tokenFace token `elem` [ElderSign, PlusOne, Zero])
        (pushAll
          [ If
            (Window.RevealTokenEffect iid token effectId)
            [LoseResources iid (AbilitySource effectSource 1) 2]
          , DisableEffect effectId
          ]
        )
      pure e
    SkillTestEnds _ _ ->
      e <$ push (DisableEffect effectId)
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> e <$ pushAll
        [ EnemyDamage eid $ nonAttack (InvestigatorSource iid) 2
        , DisableEffect effectId
        ]
    _ -> IneffibleTruth5Effect <$> runMessage msg attrs
