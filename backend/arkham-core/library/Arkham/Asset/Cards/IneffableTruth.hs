module Arkham.Asset.Cards.IneffableTruth (ineffableTruth, IneffableTruth (..), ineffableTruthEffect) where

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
import Arkham.SkillType
import Arkham.Window qualified as Window

newtype IneffableTruth = IneffableTruth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth :: AssetCard IneffableTruth
ineffableTruth = asset IneffableTruth Cards.ineffableTruth

instance HasAbilities IneffableTruth where
  getAbilities (IneffableTruth a) =
    [restrictedAbility a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage IneffableTruth where
  runMessage msg a@(IneffableTruth attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseEvade <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade iid source)
      pushAll
        $ [ createCardEffect Cards.ineffableTruth Nothing source iid
          , createCardEffect Cards.ineffableTruth Nothing source SkillTestTarget
          ]
        <> chooseEvade
      pure a
    _ -> IneffableTruth <$> runMessage msg attrs

newtype IneffableTruthEffect = IneffableTruthEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruthEffect :: EffectArgs -> IneffableTruthEffect
ineffableTruthEffect = cardEffect IneffableTruthEffect Cards.ineffableTruth

instance RunMessage IneffableTruthEffect where
  runMessage msg e@(IneffableTruthEffect attrs) = case msg of
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
    _ -> IneffableTruthEffect <$> runMessage msg attrs
