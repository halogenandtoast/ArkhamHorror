module Arkham.Asset.Cards.RiteOfSeeking (
  riteOfSeeking,
  riteOfSeekingEffect,
  RiteOfSeeking (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Message qualified as Msg
import Arkham.Window qualified as Window

newtype RiteOfSeeking = RiteOfSeeking AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

riteOfSeeking :: AssetCard RiteOfSeeking
riteOfSeeking = asset RiteOfSeeking Cards.riteOfSeeking

instance HasAbilities RiteOfSeeking where
  getAbilities (RiteOfSeeking a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage RiteOfSeeking where
  runMessage msg a@(RiteOfSeeking attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate iid source)

      pushAll
        $ createCardEffect Cards.riteOfSeeking Nothing source (InvestigationTarget iid lid)
        : leftOr investigation
      pure a
    _ -> RiteOfSeeking <$> runMessage msg attrs

newtype RiteOfSeekingEffect = RiteOfSeekingEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

riteOfSeekingEffect :: EffectArgs -> RiteOfSeekingEffect
riteOfSeekingEffect = cardEffect RiteOfSeekingEffect Cards.riteOfSeeking

instance RunMessage RiteOfSeekingEffect where
  runMessage msg e@(RiteOfSeekingEffect attrs@EffectAttrs {..}) = case msg of
    Msg.RevealChaosToken _ iid token -> case effectTarget of
      InvestigationTarget iid' _ | iid == iid' -> do
        when
          (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
          ( pushAll
              [ If
                  (Window.RevealChaosTokenEffect iid token effectId)
                  [SetActions iid effectSource 0, ChooseEndTurn iid]
              , DisableEffect effectId
              ]
          )
        pure e
      _ -> pure e
    SkillTestEnds _ _ -> do
      case effectTarget of
        InvestigatorTarget iid -> pushAll [DisableEffect effectId, EndTurn iid]
        _ -> push (DisableEffect effectId)
      pure e
    Successful (Action.Investigate, _) iid source _ _ | effectSource == source -> do
      case effectTarget of
        InvestigationTarget _ lid' -> do
          push
            (InvestigatorDiscoverClues iid lid' (toSource attrs) 1 (Just Action.Investigate))
          pure e
        _ -> pure e
    _ -> RiteOfSeekingEffect <$> runMessage msg attrs
