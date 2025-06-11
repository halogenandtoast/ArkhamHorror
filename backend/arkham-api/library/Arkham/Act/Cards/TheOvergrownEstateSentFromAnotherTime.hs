module Arkham.Act.Cards.TheOvergrownEstateSentFromAnotherTime (theOvergrownEstateSentFromAnotherTime) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheOvergrownEstateSentFromAnotherTime = TheOvergrownEstateSentFromAnotherTime ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOvergrownEstateSentFromAnotherTime :: ActCard TheOvergrownEstateSentFromAnotherTime
theOvergrownEstateSentFromAnotherTime = act (1, A) TheOvergrownEstateSentFromAnotherTime Cards.theOvergrownEstateSentFromAnotherTime Nothing

instance RunMessage TheOvergrownEstateSentFromAnotherTime where
  runMessage msg a@(TheOvergrownEstateSentFromAnotherTime attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheOvergrownEstateSentFromAnotherTime <$> liftRunMessage msg attrs
