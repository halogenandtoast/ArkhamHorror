module Arkham.Asset.Assets.TheCodexOfAges (theCodexOfAges) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype TheCodexOfAges = TheCodexOfAges AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCodexOfAges :: AssetCard TheCodexOfAges
theCodexOfAges = asset TheCodexOfAges Cards.theCodexOfAges

instance HasModifiersFor TheCodexOfAges where
  getModifiersFor (TheCodexOfAges a) = for_ a.controller \iid -> do
    modifiedWhen_ a (notNull a.sealedChaosTokens) iid [SkillModifier #willpower 1]

instance HasAbilities TheCodexOfAges where
  getAbilities (TheCodexOfAges a) =
    [restricted a 1 ControlsThis $ freeReaction (WouldRevealChaosToken #when You)]

instance RunMessage TheCodexOfAges where
  runMessage msg a@(TheCodexOfAges attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let mElderSignToken = find ((== ElderSign) . chaosTokenFace) attrs.sealedChaosTokens
      for_ mElderSignToken \token -> do
        push $ ForceChaosTokenDrawToken token
        toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> TheCodexOfAges <$> liftRunMessage msg attrs
