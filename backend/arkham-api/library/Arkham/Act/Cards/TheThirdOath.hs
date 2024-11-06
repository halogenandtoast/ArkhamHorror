module Arkham.Act.Cards.TheThirdOath
  ( TheThirdOath(..)
  , theThirdOath
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheThirdOath = TheThirdOath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theThirdOath :: ActCard TheThirdOath
theThirdOath = act (3, A) TheThirdOath Cards.theThirdOath Nothing

instance RunMessage TheThirdOath where
  runMessage msg a@(TheThirdOath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheThirdOath <$> liftRunMessage msg attrs
