module Arkham.Act.Cards.TheAscent (theAscent) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheAscent = TheAscent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theAscent :: ActCard TheAscent
theAscent = act (3, A) TheAscent Cards.theAscent Nothing

instance RunMessage TheAscent where
  runMessage msg a@(TheAscent attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheAscent <$> liftRunMessage msg attrs
