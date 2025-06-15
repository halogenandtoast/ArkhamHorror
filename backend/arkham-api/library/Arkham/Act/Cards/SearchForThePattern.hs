module Arkham.Act.Cards.SearchForThePattern (searchForThePattern) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype SearchForThePattern = SearchForThePattern ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForThePattern :: ActCard SearchForThePattern
searchForThePattern =
  act
    (1, A)
    SearchForThePattern
    Cards.searchForThePattern
    (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage SearchForThePattern where
  runMessage msg a@(SearchForThePattern attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      theWingedSerpent <- genEncounterCard Enemies.theWingedSerpent
      lead <- getLead
      drawCard lead theWingedSerpent
      advanceActDeck attrs
      pure a
    _ -> SearchForThePattern <$> liftRunMessage msg attrs
