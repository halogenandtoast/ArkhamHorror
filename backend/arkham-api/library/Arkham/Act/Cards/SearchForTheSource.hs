module Arkham.Act.Cards.SearchForTheSource (searchForTheSource) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SearchForTheSource = SearchForTheSource ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheSource :: ActCard SearchForTheSource
searchForTheSource = act (1, A) SearchForTheSource Cards.searchForTheSource Nothing

instance RunMessage SearchForTheSource where
  runMessage msg a@(SearchForTheSource attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SearchForTheSource <$> liftRunMessage msg attrs
