module Arkham.Act.Cards.SearchForTheTalisman (searchForTheTalisman) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SearchForTheTalisman = SearchForTheTalisman ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheTalisman :: ActCard SearchForTheTalisman
searchForTheTalisman = act (2, A) SearchForTheTalisman Cards.searchForTheTalisman Nothing

instance RunMessage SearchForTheTalisman where
  runMessage msg a@(SearchForTheTalisman attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SearchForTheTalisman <$> liftRunMessage msg attrs
