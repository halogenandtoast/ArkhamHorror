module Arkham.Location.Cards.EntryHall (
  entryHall,
  EntryHall (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype EntryHall = EntryHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

entryHall :: LocationCard EntryHall
entryHall =
  locationWith
    EntryHall
    Cards.entryHall
    2
    (Static 0)
    ( (connectedMatchersL <>~ [LocationWithTrait GroundFloor])
        . (revealedConnectedMatchersL <>~ [LocationWithTrait GroundFloor])
    )

instance HasAbilities EntryHall where
  getAbilities (EntryHall attrs) = withResignAction attrs []

instance RunMessage EntryHall where
  runMessage msg (EntryHall attrs) = EntryHall <$> runMessage msg attrs
