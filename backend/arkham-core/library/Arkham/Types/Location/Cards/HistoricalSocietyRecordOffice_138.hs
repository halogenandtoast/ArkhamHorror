module Arkham.Types.Location.Cards.HistoricalSocietyRecordOffice_138
  ( historicalSocietyRecordOffice_138
  , HistoricalSocietyRecordOffice_138(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyRecordOffice_138 = HistoricalSocietyRecordOffice_138 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyRecordOffice_138
  :: LocationCard HistoricalSocietyRecordOffice_138
historicalSocietyRecordOffice_138 = location
  HistoricalSocietyRecordOffice_138
  Cards.historicalSocietyRecordOffice_138
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyRecordOffice_138

instance HasAbilities HistoricalSocietyRecordOffice_138 where
  getAbilities (HistoricalSocietyRecordOffice_138 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyRecordOffice_138 where
  runMessage msg (HistoricalSocietyRecordOffice_138 attrs) =
    HistoricalSocietyRecordOffice_138 <$> runMessage msg attrs
