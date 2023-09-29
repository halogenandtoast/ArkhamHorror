module Arkham.Asset.Cards.Clairvoyance (
  clairvoyance,
  clairvoyanceEffect,
  Clairvoyance (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Investigate
import Arkham.Window qualified as Window

newtype Clairvoyance = Clairvoyance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance :: AssetCard Clairvoyance
clairvoyance = asset Clairvoyance Cards.clairvoyance

instance HasAbilities Clairvoyance where
  getAbilities (Clairvoyance a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage Clairvoyance where
  runMessage msg a@(Clairvoyance attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = toAbilitySource attrs 1
      investigation <- aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate iid source)

      pushAll
        $ [ createCardEffect Cards.clairvoyance Nothing source iid
          , skillTestModifier attrs iid (DiscoveredClues 1)
          ]
        <> leftOr investigation
      pure a
    _ -> Clairvoyance <$> runMessage msg attrs

newtype ClairvoyanceEffect = ClairvoyanceEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyanceEffect :: EffectArgs -> ClairvoyanceEffect
clairvoyanceEffect = cardEffect ClairvoyanceEffect Cards.clairvoyance

instance RunMessage ClairvoyanceEffect where
  runMessage msg e@(ClairvoyanceEffect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == effectTarget -> do
      when (chaosTokenFace token `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If (Window.RevealChaosTokenEffect iid token effectId) [assignHorror iid effectSource 1]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> ClairvoyanceEffect <$> runMessage msg attrs
