module Arkham.Asset.Assets.TheChthonianStone3 (
  theChthonianStone3,
  TheChthonianStone3 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype TheChthonianStone3 = TheChthonianStone3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChthonianStone3 :: AssetCard TheChthonianStone3
theChthonianStone3 = assetWith TheChthonianStone3 Cards.theChthonianStone3 (whenNoUsesL ?~ ReturnToHandWhenNoUses)

instance HasAbilities TheChthonianStone3 where
  getAbilities (TheChthonianStone3 a) =
    [ controlled a 1 (DuringSkillTest AnySkillTest)
        $ forced
        $ RevealChaosToken #when You
        $ ChaosTokenFaceIs AutoFail
    ]

instance RunMessage TheChthonianStone3 where
  runMessage msg a@(TheChthonianStone3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      push
        $ If
          (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs))
          [SpendUses (attrs.ability 1) (toTarget attrs) Charge 1]
      pure a
    _ -> TheChthonianStone3 <$> runMessage msg attrs
