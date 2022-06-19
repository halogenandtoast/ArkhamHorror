module Arkham.Act.Cards.OpenThePathBelow
  ( OpenThePathBelow(..)
  , openThePathBelow
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype OpenThePathBelow = OpenThePathBelow ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openThePathBelow :: ActCard OpenThePathBelow
openThePathBelow = act (1, A) OpenThePathBelow Cards.openThePathBelow Nothing

instance RunMessage OpenThePathBelow where
  runMessage msg (OpenThePathBelow attrs) = OpenThePathBelow <$> runMessage msg attrs
