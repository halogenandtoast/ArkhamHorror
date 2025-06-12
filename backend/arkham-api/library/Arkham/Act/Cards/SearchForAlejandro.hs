module Arkham.Act.Cards.SearchForAlejandro (searchForAlejandro) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype SearchForAlejandro = SearchForAlejandro ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForAlejandro :: ActCard SearchForAlejandro
searchForAlejandro =
  act (1, C) SearchForAlejandro Cards.searchForAlejandro
    $ Just
    $ GroupClueCost (PerPlayer 1)
    $ LocationWithTitle "Easttown"

instance RunMessage SearchForAlejandro where
  runMessage msg a@(SearchForAlejandro attrs) = runQueueT $ case msg of
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      velmasDiner <- selectJust $ locationIs Locations.velmasDiner
      henryDeveau <- genCard Assets.henryDeveau
      createAssetAt_ henryDeveau (AtLocation velmasDiner)
      advanceActDeck attrs
      pure a
    _ -> SearchForAlejandro <$> liftRunMessage msg attrs
