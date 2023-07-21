module Arkham.Act.Cards.BeyondTheMistV1 (
  BeyondTheMistV1 (..),
  beyondTheMistV1,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype BeyondTheMistV1 = BeyondTheMistV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondTheMistV1 :: ActCard BeyondTheMistV1
beyondTheMistV1 = act (3, A) BeyondTheMistV1 Cards.beyondTheMistV1 Nothing

instance RunMessage BeyondTheMistV1 where
  runMessage msg (BeyondTheMistV1 attrs) = BeyondTheMistV1 <$> runMessage msg attrs
