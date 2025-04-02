module Arkham.Act.Cards.TheGreatSeal (theGreatSeal) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheGreatSeal = TheGreatSeal ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGreatSeal :: ActCard TheGreatSeal
theGreatSeal = act (2, A) TheGreatSeal Cards.theGreatSeal Nothing

instance RunMessage TheGreatSeal where
  runMessage msg a@(TheGreatSeal attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheGreatSeal <$> liftRunMessage msg attrs
