module Arkham.Types.Location.Cards.HistoricalSocietyHistoricalLibrary_136
  ( historicalSocietyHistoricalLibrary_136
  , HistoricalSocietyHistoricalLibrary_136(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyHistoricalLibrary_136 = HistoricalSocietyHistoricalLibrary_136 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalLibrary_136
  :: LocationCard HistoricalSocietyHistoricalLibrary_136
historicalSocietyHistoricalLibrary_136 = location
  HistoricalSocietyHistoricalLibrary_136
  Cards.historicalSocietyHistoricalLibrary_136
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyHistoricalLibrary_136

instance HasAbilities HistoricalSocietyHistoricalLibrary_136 where
  getAbilities (HistoricalSocietyHistoricalLibrary_136 attrs) =
    getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyHistoricalLibrary_136 where
  runMessage msg (HistoricalSocietyHistoricalLibrary_136 attrs) =
    HistoricalSocietyHistoricalLibrary_136 <$> runMessage msg attrs
