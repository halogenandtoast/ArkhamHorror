module Arkham.Act.Cards.SearchForTheSource (searchForTheSource) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Placement

newtype SearchForTheSource = SearchForTheSource ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheSource :: ActCard SearchForTheSource
searchForTheSource =
  act (1, G) SearchForTheSource Cards.searchForTheSource
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Northside"

instance RunMessage SearchForTheSource where
  runMessage msg a@(SearchForTheSource attrs) = runQueueT $ case msg of
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      miskatonicUniversity <- selectJust $ location_ "Miskatonic University"
      createAssetAt_ Assets.merleGarvinUnhelpfulGuide (AtLocation miskatonicUniversity)
      advanceActDeck attrs
      pure a
    _ -> SearchForTheSource <$> liftRunMessage msg attrs
