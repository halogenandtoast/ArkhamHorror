module Arkham.Act.Cards.SeekOutTheNight (SeekOutTheNight (..), seekOutTheNight) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Prelude

newtype SeekOutTheNight = SeekOutTheNight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

seekOutTheNight :: ActCard SeekOutTheNight
seekOutTheNight = act (2, A) SeekOutTheNight Cards.seekOutTheNight Nothing

instance RunMessage SeekOutTheNight where
  runMessage msg (SeekOutTheNight attrs) = SeekOutTheNight <$> runMessage msg attrs
