module Arkham.Act.Cards.HarlanIsInDanger (harlanIsInDanger) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Placement

newtype HarlanIsInDanger = HarlanIsInDanger ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harlanIsInDanger :: ActCard HarlanIsInDanger
harlanIsInDanger =
  act (1, A) HarlanIsInDanger Cards.harlanIsInDanger
    $ Just
    $ GroupClueCost (PerPlayer 2) "Miskatonic University"

instance RunMessage HarlanIsInDanger where
  runMessage msg a@(HarlanIsInDanger attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      easttown <- selectJust $ LocationWithTitle "Easttown"
      createAssetAt_ Assets.harlanEarnstone (AtLocation easttown)
      advanceActDeck attrs
      pure a
    _ -> HarlanIsInDanger <$> liftRunMessage msg attrs
