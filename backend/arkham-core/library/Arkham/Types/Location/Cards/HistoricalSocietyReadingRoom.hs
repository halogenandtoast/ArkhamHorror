module Arkham.Types.Location.Cards.HistoricalSocietyReadingRoom
  ( historicalSocietyReadingRoom
  , HistoricalSocietyReadingRoom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyReadingRoom = HistoricalSocietyReadingRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyReadingRoom :: LocationCard HistoricalSocietyReadingRoom
historicalSocietyReadingRoom = location
  HistoricalSocietyReadingRoom
  Cards.historicalSocietyReadingRoom
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyReadingRoom

instance HasAbilities HistoricalSocietyReadingRoom where
  getAbilities (HistoricalSocietyReadingRoom attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyReadingRoom where
  runMessage msg (HistoricalSocietyReadingRoom attrs) =
    HistoricalSocietyReadingRoom <$> runMessage msg attrs
