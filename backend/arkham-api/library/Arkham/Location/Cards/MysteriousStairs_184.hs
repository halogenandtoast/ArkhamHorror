module Arkham.Location.Cards.MysteriousStairs_184 (
  mysteriousStairs_184,
  MysteriousStairs_184 (..),
)
where

import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.Projection

newtype MysteriousStairs_184 = MysteriousStairs_184 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mysteriousStairs_184 :: LocationCard MysteriousStairs_184
mysteriousStairs_184 =
  locationWith
    MysteriousStairs_184
    Cards.mysteriousStairs_184
    4
    (PerPlayer 1)
    (connectsToL .~ setFromList [Above, Below])

instance HasModifiersFor MysteriousStairs_184 where
  getModifiersFor (InvestigatorTarget iid) (MysteriousStairs_184 attrs) = do
    here <- iid `isAt` attrs
    hasClues <- fieldSome LocationClues attrs.id
    pure
      $ toModifiers attrs
      $ guard (here && hasClues)
      *> [CannotTakeAction #move, CannotTakeAction #resign]
  getModifiersFor _ _ = pure []

instance RunMessage MysteriousStairs_184 where
  runMessage msg (MysteriousStairs_184 attrs) =
    MysteriousStairs_184 <$> runMessage msg attrs
