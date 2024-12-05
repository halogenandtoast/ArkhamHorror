module Arkham.Location.Cards.EntryHallSpectral (entryHallSpectral, EntryHallSpectral (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (SilverTwilight))

newtype EntryHallSpectral = EntryHallSpectral LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHallSpectral :: LocationCard EntryHallSpectral
entryHallSpectral = location EntryHallSpectral Cards.entryHallSpectral 3 (Static 0)

instance HasModifiersFor EntryHallSpectral where
  getModifiersFor (EntryHallSpectral a) =
    if a.revealed
      then modifySelect a (EnemyWithTrait SilverTwilight) [CannotSpawnIn (be a)]
      else modifySelf a [Blocked]

instance HasAbilities EntryHallSpectral where
  getAbilities (EntryHallSpectral a) =
    extendRevealed1 a
      $ withTooltip
        "You tear through the front doors of the manor, escaping the spectral realm and leaving the remainder of the survivors to their fate."
        (locationResignAction a)

instance RunMessage EntryHallSpectral where
  runMessage msg (EntryHallSpectral attrs) = EntryHallSpectral <$> runMessage msg attrs
