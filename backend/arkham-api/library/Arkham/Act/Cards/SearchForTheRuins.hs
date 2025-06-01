module Arkham.Act.Cards.SearchForTheRuins (searchForTheRuins) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype SearchForTheRuins = SearchForTheRuins ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheRuins :: ActCard SearchForTheRuins
searchForTheRuins =
  act
    (3, A)
    SearchForTheRuins
    Cards.searchForTheRuins
    (Just $ GroupClueCost (PerPlayer 3) "Ruins of Eztli")

instance HasModifiersFor SearchForTheRuins where
  getModifiersFor (SearchForTheRuins a) = do
    modifySelect a (EnemyWithTitle "Eztli Guardian") [CannotAttack, CannotBeAttacked]
    modifySelect a (treacheryIs Treacheries.arrowsFromTheTrees) [IgnoreRevelation]

instance RunMessage SearchForTheRuins where
  runMessage msg a@(SearchForTheRuins attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> SearchForTheRuins <$> liftRunMessage msg attrs
