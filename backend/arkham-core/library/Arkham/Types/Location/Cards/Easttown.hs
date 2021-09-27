module Arkham.Types.Location.Cards.Easttown where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (easttown)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait

newtype Easttown = Easttown LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

easttown :: LocationCard Easttown
easttown =
  location Easttown Cards.easttown 2 (PerPlayer 1) Moon [Circle, Triangle]

instance HasModifiersFor env Easttown where
  getModifiersFor _ (InvestigatorTarget iid) (Easttown attrs) =
    pure $ toModifiers
      attrs
      [ ReduceCostOf (CardWithTrait Ally) 2
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage env Easttown where
  runMessage msg (Easttown attrs) = Easttown <$> runMessage msg attrs
