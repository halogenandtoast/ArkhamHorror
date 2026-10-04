module Arkham.Homebrew.AgainstTheWendigo.Locations.TempleOfIthaqua (templeOfIthaqua) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TempleOfIthaqua = TempleOfIthaqua LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfIthaqua :: LocationCard TempleOfIthaqua
templeOfIthaqua = location TempleOfIthaqua Cards.templeOfIthaqua 3 (Static 0)

instance HasModifiersFor TempleOfIthaqua where
  -- The unrevealed Mountain Range prints "You cannot move into the Mountain Range."
  getModifiersFor (TempleOfIthaqua a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance RunMessage TempleOfIthaqua where
  runMessage msg (TempleOfIthaqua attrs) = runQueueT $ TempleOfIthaqua <$> liftRunMessage msg attrs
