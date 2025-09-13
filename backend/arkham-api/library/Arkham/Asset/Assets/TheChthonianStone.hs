module Arkham.Asset.Assets.TheChthonianStone (theChthonianStone) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype TheChthonianStone = TheChthonianStone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChthonianStone :: AssetCard TheChthonianStone
theChthonianStone = asset TheChthonianStone Cards.theChthonianStone

instance HasAbilities TheChthonianStone where
  getAbilities (TheChthonianStone a) =
    [controlled a 1 (DuringSkillTest AnySkillTest) $ forced $ RevealChaosToken #after You #autofail]

instance RunMessage TheChthonianStone where
  runMessage msg a@(TheChthonianStone attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> tokens) _ -> do
      push
        $ If
          (Window.RevealChaosTokenAssetAbilityEffect iid tokens (toId attrs))
          [ReturnToHand iid (toTarget attrs)]
      pure a
    _ -> TheChthonianStone <$> liftRunMessage msg attrs
