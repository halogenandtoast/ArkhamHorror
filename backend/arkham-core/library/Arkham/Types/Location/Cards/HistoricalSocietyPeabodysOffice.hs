module Arkham.Types.Location.Cards.HistoricalSocietyPeabodysOffice
  ( historicalSocietyPeabodysOffice
  , HistoricalSocietyPeabodysOffice(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyPeabodysOffice = HistoricalSocietyPeabodysOffice LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyPeabodysOffice :: LocationCard HistoricalSocietyPeabodysOffice
historicalSocietyPeabodysOffice = location
  HistoricalSocietyPeabodysOffice
  Cards.historicalSocietyPeabodysOffice
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyPeabodysOffice

instance HasAbilities HistoricalSocietyPeabodysOffice where
  getAbilities (HistoricalSocietyPeabodysOffice attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyPeabodysOffice where
  runMessage msg (HistoricalSocietyPeabodysOffice attrs) =
    HistoricalSocietyPeabodysOffice <$> runMessage msg attrs
