module Arkham.Location.Cards.TheTwistedHollow (theTwistedHollow) where

import Arkham.Ability
import Arkham.Act.Sequence
import Arkham.Act.Cards qualified as Acts
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheTwistedHollow = TheTwistedHollow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTwistedHollow :: LocationCard TheTwistedHollow
theTwistedHollow = locationWith TheTwistedHollow Cards.theTwistedHollow 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities TheTwistedHollow where
  getAbilities (TheTwistedHollow a) =
    extendRevealed1 a
      $ mkAbility a 1 (Objective $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 2) (be a)))

instance RunMessage TheTwistedHollow where
  runMessage msg l@(TheTwistedHollow attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ AdvanceToAct 1 Acts.wheresBertie B (toSource attrs)
      pure l
    _ -> TheTwistedHollow <$> liftRunMessage msg attrs
