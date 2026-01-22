module Arkham.Act.Cards.DescentIntoTheMines (descentIntoTheMines) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype DescentIntoTheMines = DescentIntoTheMines ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

descentIntoTheMines :: ActCard DescentIntoTheMines
descentIntoTheMines = act (1, A) DescentIntoTheMines Cards.descentIntoTheMines Nothing

instance RunMessage DescentIntoTheMines where
  runMessage msg a@(DescentIntoTheMines attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> DescentIntoTheMines <$> liftRunMessage msg attrs
