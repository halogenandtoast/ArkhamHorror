module Arkham.Homebrew.AgainstTheWendigo.Locations.SiteOfAncientStones (siteOfAncientStones) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SiteOfAncientStones = SiteOfAncientStones LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

siteOfAncientStones :: LocationCard SiteOfAncientStones
siteOfAncientStones = location SiteOfAncientStones Cards.siteOfAncientStones 4 (PerPlayer 2)

instance HasModifiersFor SiteOfAncientStones where
  -- The unrevealed side prints "You cannot move into this location."
  getModifiersFor (SiteOfAncientStones a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance RunMessage SiteOfAncientStones where
  runMessage msg (SiteOfAncientStones attrs) = runQueueT $ SiteOfAncientStones <$> liftRunMessage msg attrs
