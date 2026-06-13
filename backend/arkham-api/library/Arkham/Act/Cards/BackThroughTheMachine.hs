module Arkham.Act.Cards.BackThroughTheMachine (backThroughTheMachine) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype BackThroughTheMachine = BackThroughTheMachine ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backThroughTheMachine :: ActCard BackThroughTheMachine
backThroughTheMachine = act (2, A) BackThroughTheMachine Cards.backThroughTheMachine Nothing

-- TODO: abilities
instance HasAbilities BackThroughTheMachine where
  getAbilities _ = []

instance RunMessage BackThroughTheMachine where
  runMessage msg (BackThroughTheMachine attrs) = runQueueT $ BackThroughTheMachine <$> liftRunMessage msg attrs
