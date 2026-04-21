module Arkham.Act.Cards.TheHeartOfTheHouse (theHeartOfTheHouse) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheHeartOfTheHouse = TheHeartOfTheHouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theHeartOfTheHouse :: ActCard TheHeartOfTheHouse
theHeartOfTheHouse = act (2, A) TheHeartOfTheHouse Cards.theHeartOfTheHouse Nothing

instance RunMessage TheHeartOfTheHouse where
  runMessage msg a@(TheHeartOfTheHouse attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheHeartOfTheHouse <$> liftRunMessage msg attrs
