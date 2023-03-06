module Arkham.Location.Cards.EntryHallSpectral
  ( entryHallSpectral
  , EntryHallSpectral(..)
  ) where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait(SilverTwilight))

newtype EntryHallSpectral = EntryHallSpectral LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHallSpectral :: LocationCard EntryHallSpectral
entryHallSpectral =
  location EntryHallSpectral Cards.entryHallSpectral 3 (Static 0)

instance HasModifiersFor EntryHallSpectral where
  getModifiersFor target (EntryHallSpectral attrs) | isTarget attrs target =
    pure $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor (EnemyTarget eid) (EntryHallSpectral attrs) = do
    isSilverTwilight <- member eid <$> select (EnemyWithTrait SilverTwilight)
    pure $ toModifiers attrs [ CannotSpawnIn (LocationWithId $ toId attrs) | isSilverTwilight ]
  getModifiersFor _ _ = pure []

instance HasAbilities EntryHallSpectral where
  getAbilities (EntryHallSpectral a) = withBaseAbilities
    a
    [ withTooltip
        "You tear through the front doors of the manor, escaping the spectral realm and leaving the remainder of the survivors to their fate."
        (locationResignAction a)
    | locationRevealed a
    ]

instance RunMessage EntryHallSpectral where
  runMessage msg (EntryHallSpectral attrs) =
    EntryHallSpectral <$> runMessage msg attrs
