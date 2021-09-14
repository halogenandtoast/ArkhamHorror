module Arkham.Types.Location.Cards.HistoricalSocietyHistoricalMuseum_130
  ( historicalSocietyHistoricalMuseum_130
  , HistoricalSocietyHistoricalMuseum_130(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyHistoricalMuseum_130 = HistoricalSocietyHistoricalMuseum_130 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalMuseum_130
  :: LocationCard HistoricalSocietyHistoricalMuseum_130
historicalSocietyHistoricalMuseum_130 = location
  HistoricalSocietyHistoricalMuseum_130
  Cards.historicalSocietyHistoricalMuseum_130
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyHistoricalMuseum_130

instance HasAbilities HistoricalSocietyHistoricalMuseum_130 where
  getAbilities (HistoricalSocietyHistoricalMuseum_130 attrs) =
    getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyHistoricalMuseum_130 where
  runMessage msg (HistoricalSocietyHistoricalMuseum_130 attrs) =
    HistoricalSocietyHistoricalMuseum_130 <$> runMessage msg attrs
