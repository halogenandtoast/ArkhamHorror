module Arkham.Asset.Cards.RiteOfSeeking (
  riteOfSeeking,
  riteOfSeekingEffect,
  RiteOfSeeking (..),
) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Window qualified as Window

newtype RiteOfSeeking = RiteOfSeeking AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: AssetCard RiteOfSeeking
riteOfSeeking = asset RiteOfSeeking Cards.riteOfSeeking

instance HasAbilities RiteOfSeeking where
  getAbilities (RiteOfSeeking a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage RiteOfSeeking where
  runMessage msg a@(RiteOfSeeking attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)

      createCardEffect Cards.riteOfSeeking (effectMetaTarget sid) source (InvestigationTarget iid lid)
      skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pushAll $ leftOr investigation
      pure a
    _ -> RiteOfSeeking <$> liftRunMessage msg attrs

newtype RiteOfSeekingEffect = RiteOfSeekingEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeekingEffect :: EffectArgs -> RiteOfSeekingEffect
riteOfSeekingEffect = cardEffect RiteOfSeekingEffect Cards.riteOfSeeking

instance RunMessage RiteOfSeekingEffect where
  runMessage msg e@(RiteOfSeekingEffect attrs) = runQueueT $ case msg of
    Msg.RevealChaosToken (SkillTestSource sid) iid token | maybe False (isTarget sid) attrs.metaTarget -> do
      case attrs.target of
        InvestigationTarget iid' _ | iid == iid' -> do
          when (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) do
            push
              $ If
                (Window.RevealChaosTokenEffect iid token attrs.id)
                [SetActions iid attrs.source 0, ChooseEndTurn iid]
            disable attrs
        _ -> pure ()
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disable attrs
      case attrs.target of
        InvestigatorTarget iid -> push $ EndTurn iid
        _ -> pure ()
      pure e
    _ -> RiteOfSeekingEffect <$> liftRunMessage msg attrs
