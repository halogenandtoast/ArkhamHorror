module Arkham.Act.Cards.SearchingTheHeath (searchingTheHeath) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SearchingTheHeath = SearchingTheHeath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchingTheHeath :: ActCard SearchingTheHeath
searchingTheHeath = act (2, A) SearchingTheHeath Cards.searchingTheHeath Nothing

instance RunMessage SearchingTheHeath where
  runMessage msg a@(SearchingTheHeath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SearchingTheHeath <$> liftRunMessage msg attrs
