module Arkham.Asset.Cards.IneffableTruth3 (ineffableTruth3, IneffableTruth3 (..), ineffableTruth3Effect) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Evade
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype IneffableTruth3 = IneffableTruth3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth3 :: AssetCard IneffableTruth3
ineffableTruth3 = asset IneffableTruth3 Cards.ineffableTruth3

instance HasAbilities IneffableTruth3 where
  getAbilities (IneffableTruth3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ evadeAction
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffableTruth3 where
  runMessage msg a@(IneffableTruth3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseEvade <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade iid source)
      pushAll
        $ [ createCardEffect Cards.ineffableTruth3 Nothing source iid
          , createCardEffect Cards.ineffableTruth3 Nothing source SkillTestTarget
          , skillTestModifier source iid (SkillModifier #willpower 2)
          ]
        <> chooseEvade
      pure a
    _ -> IneffableTruth3 <$> runMessage msg attrs

newtype IneffableTruth3Effect = IneffableTruth3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth3Effect :: EffectArgs -> IneffableTruth3Effect
ineffableTruth3Effect = cardEffect IneffableTruth3Effect Cards.ineffableTruth3

instance RunMessage IneffableTruth3Effect where
  runMessage msg e@(IneffableTruth3Effect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      when
        (token.face `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token attrs.id)
              [LoseResources iid attrs.source 1]
          , disable attrs
          ]
      pure e
    SkillTestEnds _ _ ->
      e <$ push (disable attrs)
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == attrs.target -> do
          pushAll
            [ EnemyDamage eid $ nonAttack iid 1
            , disable attrs
            ]
          pure e
    _ -> IneffableTruth3Effect <$> runMessage msg attrs
