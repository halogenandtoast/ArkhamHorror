module Arkham.Location.Cards.Easttown where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (easttown)
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype Easttown = Easttown LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

easttown :: LocationCard Easttown
easttown = location Easttown Cards.easttown 2 (PerPlayer 1)

instance HasModifiersFor Easttown where
  getModifiersFor (InvestigatorTarget iid) (Easttown attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs $ [ReduceCostOf (#asset <> #ally) 2 | here]
  getModifiersFor _ _ = pure []

instance RunMessage Easttown where
  runMessage msg (Easttown attrs) = Easttown <$> runMessage msg attrs
