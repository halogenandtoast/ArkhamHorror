module Arkham.Location.Cards.Theatre
  ( theatre
  , Theatre(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs

newtype Theatre = Theatre LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theatre :: LocationCard Theatre
theatre =
  location Theatre Cards.theatre 2 (Static 0) Circle [Diamond, Triangle]

instance LocationRunner env => RunMessage env Theatre where
  runMessage msg (Theatre attrs) = Theatre <$> runMessage msg attrs
