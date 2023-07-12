module Arkham.Act.Cards.BeyondTheMistV2 (
  BeyondTheMistV2 (..),
  beyondTheMistV2,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype BeyondTheMistV2 = BeyondTheMistV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondTheMistV2 :: ActCard BeyondTheMistV2
beyondTheMistV2 = act (3, A) BeyondTheMistV2 Cards.beyondTheMistV2 Nothing

instance RunMessage BeyondTheMistV2 where
  runMessage msg (BeyondTheMistV2 attrs) = BeyondTheMistV2 <$> runMessage msg attrs
