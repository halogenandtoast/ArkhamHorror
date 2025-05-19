module Arkham.Location.Cards.EntryHall (entryHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.EchoesOfThePast.Helpers
import Arkham.Trait

newtype EntryHall = EntryHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHall :: LocationCard EntryHall
entryHall =
  locationWith EntryHall Cards.entryHall 2 (Static 0)
    $ (connectedMatchersL <>~ [LocationWithTrait GroundFloor])
    . (revealedConnectedMatchersL <>~ [LocationWithTrait GroundFloor])

instance HasAbilities EntryHall where
  getAbilities (EntryHall a) = extendRevealed1 a $ scenarioI18n $ withI18nTooltip "entryHall.resign" $ locationResignAction a

instance RunMessage EntryHall where
  runMessage msg (EntryHall attrs) = EntryHall <$> runMessage msg attrs
