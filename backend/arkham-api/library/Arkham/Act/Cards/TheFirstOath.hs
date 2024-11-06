module Arkham.Act.Cards.TheFirstOath
  ( TheFirstOath(..)
  , theFirstOath
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheFirstOath = TheFirstOath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theFirstOath :: ActCard TheFirstOath
theFirstOath = act (1, A) TheFirstOath Cards.theFirstOath Nothing

instance RunMessage TheFirstOath where
  runMessage msg a@(TheFirstOath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheFirstOath <$> liftRunMessage msg attrs
