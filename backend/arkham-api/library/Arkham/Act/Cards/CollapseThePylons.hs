module Arkham.Act.Cards.CollapseThePylons (collapseThePylons) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CollapseThePylons = CollapseThePylons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

collapseThePylons :: ActCard CollapseThePylons
collapseThePylons = act (1, A) CollapseThePylons Cards.collapseThePylons Nothing

instance RunMessage CollapseThePylons where
  runMessage msg a@(CollapseThePylons attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CollapseThePylons <$> liftRunMessage msg attrs
