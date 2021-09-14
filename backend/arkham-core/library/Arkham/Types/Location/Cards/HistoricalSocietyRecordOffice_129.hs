module Arkham.Types.Location.Cards.HistoricalSocietyRecordOffice_129
  ( historicalSocietyRecordOffice_129
  , HistoricalSocietyRecordOffice_129(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyRecordOffice_129 = HistoricalSocietyRecordOffice_129 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyRecordOffice_129
  :: LocationCard HistoricalSocietyRecordOffice_129
historicalSocietyRecordOffice_129 = location
  HistoricalSocietyRecordOffice_129
  Cards.historicalSocietyRecordOffice_129
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyRecordOffice_129

instance HasAbilities HistoricalSocietyRecordOffice_129 where
  getAbilities (HistoricalSocietyRecordOffice_129 attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyRecordOffice_129 where
  runMessage msg (HistoricalSocietyRecordOffice_129 attrs) =
    HistoricalSocietyRecordOffice_129 <$> runMessage msg attrs
