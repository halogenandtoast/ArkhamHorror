module Arkham.Types.Location.Cards.Theatre
  ( theatre
  , Theatre(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

newtype Theatre = Theatre LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theatre :: LocationCard Theatre
theatre =
  location Theatre Cards.theatre 2 (Static 0) Circle [Diamond, Triangle]

instance LocationRunner env => RunMessage env Theatre where
  runMessage msg (Theatre attrs) = Theatre <$> runMessage msg attrs
