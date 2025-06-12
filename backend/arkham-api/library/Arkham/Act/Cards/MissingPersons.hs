module Arkham.Act.Cards.MissingPersons (missingPersons) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype MissingPersons = MissingPersons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

missingPersons :: ActCard MissingPersons
missingPersons =
  act (1, C) MissingPersons Cards.missingPersons
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Easttown"

instance RunMessage MissingPersons where
  runMessage msg a@(MissingPersons attrs) = runQueueT $ case msg of
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      placeLocation_ =<< genCard Locations.arkhamPoliceStation
      advanceActDeck attrs
      pure a
    _ -> MissingPersons <$> liftRunMessage msg attrs
