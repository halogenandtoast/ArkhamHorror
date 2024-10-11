module Arkham.Asset.Assets.IneffableTruth3 (ineffableTruth3, IneffableTruth3 (..), ineffableTruth3Effect) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Effect.Import
import Arkham.Evade
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier
import Arkham.Window qualified as Window

newtype IneffableTruth3 = IneffableTruth3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth3 :: AssetCard IneffableTruth3
ineffableTruth3 = asset IneffableTruth3 Cards.ineffableTruth3

instance HasAbilities IneffableTruth3 where
  getAbilities (IneffableTruth3 a) =
    [restricted a 1 ControlsThis $ evadeAction (assetUseCost a Charge 1)]

instance RunMessage IneffableTruth3 where
  runMessage msg a@(IneffableTruth3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      createCardEffect Cards.ineffableTruth3 (effectInt 1) source sid
      createCardEffect Cards.ineffableTruth3 (effectInt 2) source sid
      skillTestModifier sid source iid (SkillModifier #willpower 2)
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    _ -> IneffableTruth3 <$> liftRunMessage msg attrs

newtype IneffableTruth3Effect = IneffableTruth3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth3Effect :: EffectArgs -> IneffableTruth3Effect
ineffableTruth3Effect = cardEffect IneffableTruth3Effect Cards.ineffableTruth3

instance RunMessage IneffableTruth3Effect where
  runMessage msg e@(IneffableTruth3Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      whenJustM getSkillTest \st -> do
        let triggers =
              and
                [ token.face `elem` [ElderSign, PlusOne, Zero]
                , iid == st.investigator
                , isTarget st attrs.target
                , attrs.metaInt == Just 1
                ]
        when triggers do
          push
            $ If
              (Window.RevealChaosTokenEffect iid token attrs.id)
              [LoseResources iid attrs.source 1]
          disable attrs
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _ -> do
      withSkillTest \sid -> do
        when (isTarget sid attrs.target && attrs.metaInt == Just 2) do
          push $ EnemyDamage eid $ nonAttack iid 1
          disable attrs
      pure e
    _ -> IneffableTruth3Effect <$> liftRunMessage msg attrs
