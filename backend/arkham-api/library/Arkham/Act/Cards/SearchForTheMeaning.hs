module Arkham.Act.Cards.SearchForTheMeaning (searchForTheMeaning) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SearchForTheMeaning = SearchForTheMeaning ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheMeaning :: ActCard SearchForTheMeaning
searchForTheMeaning = act (1, A) SearchForTheMeaning Cards.searchForTheMeaning Nothing

instance RunMessage SearchForTheMeaning where
  runMessage msg a@(SearchForTheMeaning attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SearchForTheMeaning <$> liftRunMessage msg attrs
