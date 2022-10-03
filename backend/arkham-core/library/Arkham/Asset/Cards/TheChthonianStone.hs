module Arkham.Asset.Cards.TheChthonianStone
  ( theChthonianStone
  , TheChthonianStone(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Token
import Arkham.Timing qualified as Timing

newtype TheChthonianStone = TheChthonianStone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChthonianStone :: AssetCard TheChthonianStone
theChthonianStone = asset TheChthonianStone Cards.theChthonianStone

instance HasAbilities TheChthonianStone where
  getAbilities (TheChthonianStone a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ ForcedAbility
        $ RevealChaosToken Timing.When You
        $ TokenFaceIs AutoFail
    ]

instance RunMessage TheChthonianStone where
  runMessage msg a@(TheChthonianStone attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ ReturnToHand iid (toTarget attrs)
      pure a
    _ -> TheChthonianStone <$> runMessage msg attrs
