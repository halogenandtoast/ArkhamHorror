module Arkham.Asset.Cards.Clairvoyance5 (clairvoyance5, clairvoyance5Effect, Clairvoyance5 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype Clairvoyance5 = Clairvoyance5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance5 :: AssetCard Clairvoyance5
clairvoyance5 = asset Clairvoyance5 Cards.clairvoyance5

instance HasAbilities Clairvoyance5 where
  getAbilities (Clairvoyance5 a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage Clairvoyance5 where
  runMessage msg a@(Clairvoyance5 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)

      pushAll
        $ [ createCardEffect Cards.clairvoyance5 (effectMetaTarget sid) source (InvestigationTarget iid lid)
          , skillTestModifiers sid attrs iid [DiscoveredClues 2, SkillModifier #willpower 3]
          ]
        <> leftOr investigation
      pure a
    _ -> Clairvoyance5 <$> runMessage msg attrs

newtype Clairvoyance5Effect = Clairvoyance5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance5Effect :: EffectArgs -> Clairvoyance5Effect
clairvoyance5Effect = cardEffect Clairvoyance5Effect Cards.clairvoyance5

instance RunMessage Clairvoyance5Effect where
  runMessage msg e@(Clairvoyance5Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken (SkillTestSource sid) iid token | InvestigatorTarget iid == effectTarget && maybe False (isTarget sid) attrs.metaTarget -> do
      when (chaosTokenFace token `elem` [ElderSign, PlusOne, Zero])
        $ pushAll
          [ If (Window.RevealChaosTokenEffect iid token effectId) [assignHorror iid effectSource 2]
          , DisableEffect effectId
          ]
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> e <$ push (DisableEffect effectId)
    _ -> Clairvoyance5Effect <$> runMessage msg attrs
