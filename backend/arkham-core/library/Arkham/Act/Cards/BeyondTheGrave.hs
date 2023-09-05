module Arkham.Act.Cards.BeyondTheGrave
  ( BeyondTheGrave(..)
  , beyondTheGrave
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype BeyondTheGrave = BeyondTheGrave ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondTheGrave :: ActCard BeyondTheGrave
beyondTheGrave = act (2, A) BeyondTheGrave Cards.beyondTheGrave Nothing

instance RunMessage BeyondTheGrave where
  runMessage msg (BeyondTheGrave attrs) = BeyondTheGrave <$> runMessage msg attrs
