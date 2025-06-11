module Arkham.Act.Cards.DiscoverTheTruth (discoverTheTruth) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype DiscoverTheTruth = DiscoverTheTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

discoverTheTruth :: ActCard DiscoverTheTruth
discoverTheTruth = act (1, A) DiscoverTheTruth Cards.discoverTheTruth Nothing

instance RunMessage DiscoverTheTruth where
  runMessage msg a@(DiscoverTheTruth attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> DiscoverTheTruth <$> liftRunMessage msg attrs
