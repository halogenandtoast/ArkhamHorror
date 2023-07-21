module Arkham.Act.Cards.BeyondTheMistV4 (
  BeyondTheMistV4 (..),
  beyondTheMistV4,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype BeyondTheMistV4 = BeyondTheMistV4 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondTheMistV4 :: ActCard BeyondTheMistV4
beyondTheMistV4 = act (3, A) BeyondTheMistV4 Cards.beyondTheMistV4 Nothing

instance RunMessage BeyondTheMistV4 where
  runMessage msg (BeyondTheMistV4 attrs) = BeyondTheMistV4 <$> runMessage msg attrs
