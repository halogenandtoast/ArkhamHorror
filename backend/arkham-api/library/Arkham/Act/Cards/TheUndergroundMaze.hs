module Arkham.Act.Cards.TheUndergroundMaze (theUndergroundMaze) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheUndergroundMaze = TheUndergroundMaze ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theUndergroundMaze :: ActCard TheUndergroundMaze
theUndergroundMaze = act (2, A) TheUndergroundMaze Cards.theUndergroundMaze Nothing

instance RunMessage TheUndergroundMaze where
  runMessage msg a@(TheUndergroundMaze attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheUndergroundMaze <$> liftRunMessage msg attrs
