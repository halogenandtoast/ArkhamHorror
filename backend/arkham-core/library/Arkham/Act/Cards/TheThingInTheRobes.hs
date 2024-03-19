module Arkham.Act.Cards.TheThingInTheRobes
  ( TheThingInTheRobes(..)
  , theThingInTheRobes
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheThingInTheRobes = TheThingInTheRobes ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theThingInTheRobes :: ActCard TheThingInTheRobes
theThingInTheRobes = act (2, A) TheThingInTheRobes Cards.theThingInTheRobes Nothing

instance RunMessage TheThingInTheRobes where
  runMessage msg a@(TheThingInTheRobes attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheThingInTheRobes <$> lift (runMessage msg attrs)
