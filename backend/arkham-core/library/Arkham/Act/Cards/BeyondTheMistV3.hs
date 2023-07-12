module Arkham.Act.Cards.BeyondTheMistV3
  ( BeyondTheMistV3(..)
  , beyondTheMistV3
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype BeyondTheMistV3 = BeyondTheMistV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondTheMistV3 :: ActCard BeyondTheMistV3
beyondTheMistV3 = act (3, A) BeyondTheMistV3 Cards.beyondTheMistV3 Nothing

instance RunMessage BeyondTheMistV3 where
  runMessage msg (BeyondTheMistV3 attrs) = BeyondTheMistV3 <$> runMessage msg attrs
