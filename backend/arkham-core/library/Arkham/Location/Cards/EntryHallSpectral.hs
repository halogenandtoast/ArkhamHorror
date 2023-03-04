module Arkham.Location.Cards.EntryHallSpectral
  ( entryHallSpectral
  , EntryHallSpectral(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype EntryHallSpectral = EntryHallSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHallSpectral :: LocationCard EntryHallSpectral
entryHallSpectral = location EntryHallSpectral Cards.entryHallSpectral 3 (Static 0)

instance HasAbilities EntryHallSpectral where
  getAbilities (EntryHallSpectral attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage EntryHallSpectral where
  runMessage msg (EntryHallSpectral attrs) =
    EntryHallSpectral <$> runMessage msg attrs
