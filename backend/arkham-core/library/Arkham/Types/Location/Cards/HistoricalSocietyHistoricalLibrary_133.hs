module Arkham.Types.Location.Cards.HistoricalSocietyHistoricalLibrary_133
  ( historicalSocietyHistoricalLibrary_133
  , HistoricalSocietyHistoricalLibrary_133(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyHistoricalLibrary_133 = HistoricalSocietyHistoricalLibrary_133 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalLibrary_133
  :: LocationCard HistoricalSocietyHistoricalLibrary_133
historicalSocietyHistoricalLibrary_133 = location
  HistoricalSocietyHistoricalLibrary_133
  Cards.historicalSocietyHistoricalLibrary_133
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyHistoricalLibrary_133

instance HasAbilities HistoricalSocietyHistoricalLibrary_133 where
  getAbilities (HistoricalSocietyHistoricalLibrary_133 attrs) =
    getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyHistoricalLibrary_133 where
  runMessage msg (HistoricalSocietyHistoricalLibrary_133 attrs) =
    HistoricalSocietyHistoricalLibrary_133 <$> runMessage msg attrs
