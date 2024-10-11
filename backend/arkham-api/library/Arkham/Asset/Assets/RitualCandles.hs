module Arkham.Asset.Assets.RitualCandles (ritualCandles, RitualCandles (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype RitualCandles = RitualCandles AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualCandles :: AssetCard RitualCandles
ritualCandles = asset RitualCandles Cards.ritualCandles

instance HasAbilities RitualCandles where
  getAbilities (RitualCandles x) =
    [ restricted x 1 ControlsThis
        $ freeReaction
        $ RevealChaosToken #when You
        $ if tabooed TabooList20 x
          then IsSymbol
          else ChaosTokenMatchesAny $ map ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing]
    ]

instance RunMessage RitualCandles where
  runMessage msg a@(RitualCandles attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      getSkillTestId >>= traverse_ \sid -> do
        enable <- skillTestModifier sid attrs iid (AnySkillValue 1)
        push $ If (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs)) [enable]
      pure a
    _ -> RitualCandles <$> runMessage msg attrs
