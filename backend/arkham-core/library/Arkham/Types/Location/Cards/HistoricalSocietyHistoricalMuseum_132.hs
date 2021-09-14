module Arkham.Types.Location.Cards.HistoricalSocietyHistoricalMuseum_132
  ( historicalSocietyHistoricalMuseum_132
  , HistoricalSocietyHistoricalMuseum_132(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyHistoricalMuseum_132 = HistoricalSocietyHistoricalMuseum_132 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalMuseum_132
  :: LocationCard HistoricalSocietyHistoricalMuseum_132
historicalSocietyHistoricalMuseum_132 = location
  HistoricalSocietyHistoricalMuseum_132
  Cards.historicalSocietyHistoricalMuseum_132
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyHistoricalMuseum_132

instance HasAbilities HistoricalSocietyHistoricalMuseum_132 where
  getAbilities (HistoricalSocietyHistoricalMuseum_132 attrs) =
    getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyHistoricalMuseum_132 where
  runMessage msg (HistoricalSocietyHistoricalMuseum_132 attrs) =
    HistoricalSocietyHistoricalMuseum_132 <$> runMessage msg attrs
