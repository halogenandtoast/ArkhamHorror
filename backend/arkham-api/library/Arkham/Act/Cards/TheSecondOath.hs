module Arkham.Act.Cards.TheSecondOath
  ( TheSecondOath(..)
  , theSecondOath
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheSecondOath = TheSecondOath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSecondOath :: ActCard TheSecondOath
theSecondOath = act (2, A) TheSecondOath Cards.theSecondOath Nothing

instance RunMessage TheSecondOath where
  runMessage msg a@(TheSecondOath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheSecondOath <$> liftRunMessage msg attrs
