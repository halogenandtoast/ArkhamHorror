module Arkham.Homebrew.DarkMatter.Locations.IceSpires (iceSpires) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  emptyUnstabilizedLocation,
  scanEventAt,
  shuffleLocationIntoScanningDeck,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype IceSpires = IceSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iceSpires :: LocationCard IceSpires
iceSpires = symbolLabel $ location IceSpires Cards.iceSpires 3 (PerPlayer 1)

{- | "Forced - After you scan at this location: Shuffle an empty location without
a resource token on it back into the scanning deck."
-}
instance HasAbilities IceSpires where
  getAbilities (IceSpires a) =
    extendRevealed1 a
      $ restricted a 1 (exists emptyUnstabilizedLocation)
      $ forced
      $ CampaignEvent #after (Just You) (scanEventAt a.id)

instance RunMessage IceSpires where
  runMessage msg l@(IceSpires attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      candidates <- select emptyUnstabilizedLocation
      chooseOrRunOneM iid $ targets candidates shuffleLocationIntoScanningDeck
      pure l
    _ -> IceSpires <$> liftRunMessage msg attrs
