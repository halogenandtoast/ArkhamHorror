module Arkham.Asset.Assets.RitualCandles (ritualCandles) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Modifiers qualified as Helpers
import Arkham.Helpers.SkillTest (getSkillTestId)
import Arkham.Matcher
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype RitualCandles = RitualCandles AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualCandles :: AssetCard RitualCandles
ritualCandles = asset RitualCandles Cards.ritualCandles

instance HasAbilities RitualCandles where
  getAbilities (RitualCandles x) =
    [ controlled x 1 (DuringSkillTest $ YourSkillTest AnySkillTest)
        $ freeReaction
        $ RevealChaosToken #when Anyone
        $ if tabooed TabooList20 x
          then IsSymbol
          else ChaosTokenMatchesAny $ map ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing]
    ]

instance RunMessage RitualCandles where
  runMessage msg a@(RitualCandles attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      getSkillTestId >>= traverse_ \sid -> do
        enable <- Helpers.skillTestModifier sid attrs iid (AnySkillValue 1)
        push $ If (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs)) [enable]
      pure a
    _ -> RitualCandles <$> liftRunMessage msg attrs
