module Arkham.Types.Location.Cards.EntryHall
  ( entryHall
  , EntryHall(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype EntryHall = EntryHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHall :: LocationCard EntryHall
entryHall = location EntryHall Cards.entryHall 0 (Static 0) NoSymbol []

instance HasModifiersFor env EntryHall

instance HasAbilities EntryHall where
  getAbilities (EntryHall attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env EntryHall where
  runMessage msg (EntryHall attrs) = EntryHall <$> runMessage msg attrs
