module Arkham.Asset.Assets.FoundationIntel (foundationIntel) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Placement

newtype FoundationIntel = FoundationIntel AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foundationIntel :: AssetCard FoundationIntel
foundationIntel = asset FoundationIntel Cards.foundationIntel

instance HasModifiersFor FoundationIntel where
  getModifiersFor (FoundationIntel a) = case a.placement of
    StillInHand _ -> modified_ a (toCard a) [CannotLeaveYourHand]
    _ -> pure ()

instance RunMessage FoundationIntel where
  runMessage msg (FoundationIntel attrs) = FoundationIntel <$> runMessage msg attrs
