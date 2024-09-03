module Arkham.Asset.Cards.TheChthonianStone (
  theChthonianStone,
  TheChthonianStone (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype TheChthonianStone = TheChthonianStone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChthonianStone :: AssetCard TheChthonianStone
theChthonianStone = asset TheChthonianStone Cards.theChthonianStone

instance HasAbilities TheChthonianStone where
  getAbilities (TheChthonianStone a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ ForcedAbility
        $ RevealChaosToken #when You
        $ ChaosTokenFaceIs AutoFail
    ]

instance RunMessage TheChthonianStone where
  runMessage msg a@(TheChthonianStone attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      push
        $ If
          (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs))
          [ReturnToHand iid (toTarget attrs)]
      pure a
    _ -> TheChthonianStone <$> runMessage msg attrs
