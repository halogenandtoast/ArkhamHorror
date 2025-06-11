module Arkham.Act.Cards.SearchForTheMeaning (searchForTheMeaning) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations

newtype SearchForTheMeaning = SearchForTheMeaning ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheMeaning :: ActCard SearchForTheMeaning
searchForTheMeaning =
  act (1, G) SearchForTheMeaning Cards.searchForTheMeaning
    $ Just
    $ GroupClueCost (PerPlayer 1) "Rivertown"

instance RunMessage SearchForTheMeaning where
  runMessage msg a@(SearchForTheMeaning attrs) = runQueueT $ case msg of
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      placeLocation_ Locations.theHastingsEstate
      advanceActDeck attrs
      pure a
    _ -> SearchForTheMeaning <$> liftRunMessage msg attrs
