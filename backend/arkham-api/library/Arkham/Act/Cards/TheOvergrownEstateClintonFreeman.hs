module Arkham.Act.Cards.TheOvergrownEstateClintonFreeman (theOvergrownEstateClintonFreeman) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Placement

newtype TheOvergrownEstateClintonFreeman = TheOvergrownEstateClintonFreeman ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOvergrownEstateClintonFreeman :: ActCard TheOvergrownEstateClintonFreeman
theOvergrownEstateClintonFreeman =
  act (2, G) TheOvergrownEstateClintonFreeman Cards.theOvergrownEstateClintonFreeman
    $ Just
    $ GroupClueCost (PerPlayer 2) "The Hastings Estate"

instance RunMessage TheOvergrownEstateClintonFreeman where
  runMessage msg a@(TheOvergrownEstateClintonFreeman attrs) = runQueueT $ case msg of
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      velmasDiner <- selectJust $ location_ "Velma's Diner"
      createAssetAt_ Assets.clintonFreemanShouldHaveStayedHome (AtLocation velmasDiner)
      advanceToAct attrs Cards.discoverTheTruth G
      pure a
    _ -> TheOvergrownEstateClintonFreeman <$> liftRunMessage msg attrs
