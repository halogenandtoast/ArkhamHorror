module Arkham.Types.Location.Cards.MessHall
  ( messHall
  , MessHall(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype MessHall = MessHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

messHall :: LocationCard MessHall
messHall =
  location MessHall Cards.messHall 2 (PerPlayer 2) Triangle [Circle, Square]

instance HasAbilities MessHall where
  getAbilities (MessHall attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env MessHall where
  runMessage msg (MessHall attrs) = MessHall <$> runMessage msg attrs
