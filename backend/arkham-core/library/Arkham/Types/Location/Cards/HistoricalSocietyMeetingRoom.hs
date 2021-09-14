module Arkham.Types.Location.Cards.HistoricalSocietyMeetingRoom
  ( historicalSocietyMeetingRoom
  , HistoricalSocietyMeetingRoom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype HistoricalSocietyMeetingRoom = HistoricalSocietyMeetingRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyMeetingRoom :: LocationCard HistoricalSocietyMeetingRoom
historicalSocietyMeetingRoom = location
  HistoricalSocietyMeetingRoom
  Cards.historicalSocietyMeetingRoom
  0
  (Static 0)
  NoSymbol
  []

instance HasModifiersFor env HistoricalSocietyMeetingRoom

instance HasAbilities HistoricalSocietyMeetingRoom where
  getAbilities (HistoricalSocietyMeetingRoom attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env HistoricalSocietyMeetingRoom where
  runMessage msg (HistoricalSocietyMeetingRoom attrs) =
    HistoricalSocietyMeetingRoom <$> runMessage msg attrs
