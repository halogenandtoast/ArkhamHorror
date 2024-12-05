module Arkham.Asset.Assets.TheCodexOfAgesAdvanced (
  theCodexOfAgesAdvanced,
  TheCodexOfAgesAdvanced (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheCodexOfAgesAdvanced = TheCodexOfAgesAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCodexOfAgesAdvanced :: AssetCard TheCodexOfAgesAdvanced
theCodexOfAgesAdvanced = asset TheCodexOfAgesAdvanced Cards.theCodexOfAgesAdvanced

instance HasModifiersFor TheCodexOfAgesAdvanced where
  getModifiersFor (TheCodexOfAgesAdvanced a) =
    controllerGets
      a
      [SkillModifier sType 1 | notNull (assetSealedChaosTokens a), sType <- [minBound ..]]

instance HasAbilities TheCodexOfAgesAdvanced where
  getAbilities (TheCodexOfAgesAdvanced a) =
    [ controlledAbility a 1 (thisExists a (AssetWithSealedChaosTokens 1 AnyChaosToken))
        $ freeReaction
        $ WouldRevealChaosToken #when (affectsOthers $ at_ YourLocation)
    ]

instance RunMessage TheCodexOfAgesAdvanced where
  runMessage msg a@(TheCodexOfAgesAdvanced attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (wouldRevealChaosToken -> iid') _ -> do
      tokens <- select $ SealedOnAsset (be attrs) AnyChaosToken
      focusChaosTokens tokens \unfocus -> do
        chooseOrRunTargetM iid' tokens \token -> do
          push unfocus
          push
            $ ReplaceCurrentDraw (attrs.ability 2) iid'
            $ Choose (attrs.ability 2) 1 ResolveChoice [Resolved [token]] [] Nothing
      pure a
    ResolveChaosToken drawnToken _ _ -> do
      whenM (drawnToken <=~> SealedOnAsset (be attrs) AnyChaosToken) do
        push $ UnsealChaosToken drawnToken
      pure a
    _ -> TheCodexOfAgesAdvanced <$> liftRunMessage msg attrs
