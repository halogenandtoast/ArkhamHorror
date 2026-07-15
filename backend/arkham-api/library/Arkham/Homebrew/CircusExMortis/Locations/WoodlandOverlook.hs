module Arkham.Homebrew.CircusExMortis.Locations.WoodlandOverlook (woodlandOverlook) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..), countTokens)

newtype WoodlandOverlook = WoodlandOverlook LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

woodlandOverlook :: LocationCard WoodlandOverlook
woodlandOverlook =
  location WoodlandOverlook Cards.woodlandOverlook 4 (Static 1)

instance HasModifiersFor WoodlandOverlook where
  getModifiersFor (WoodlandOverlook a) = do
    -- "Investigators cannot enter Woodland Overlook while there is fewer than 2 damage on it."
    let damage = countTokens Damage a.tokens
    modifySelect a Anyone [CannotEnter a.id | damage < 2]

-- TODO(homebrew): "As an additional cost to move from Woodland Overlook to a non-[[Woods]]
-- location, discard a non-weakness asset you control." Dormant/unreachable given the
-- scenario connections (Woodland Overlook only connects to the three leftmost copies of
-- Moonlit Forest, which are all [[Woods]]). No destination-filtered leave-cost primitive
-- exists, so it is intentionally not modeled.

instance RunMessage WoodlandOverlook where
  runMessage msg (WoodlandOverlook attrs) = runQueueT $ case msg of
    _ -> WoodlandOverlook <$> liftRunMessage msg attrs
