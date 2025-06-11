module Arkham.Act.Cards.TheOvergrownEstateClintonFreeman (theOvergrownEstateClintonFreeman) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheOvergrownEstateClintonFreeman = TheOvergrownEstateClintonFreeman ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOvergrownEstateClintonFreeman :: ActCard TheOvergrownEstateClintonFreeman
theOvergrownEstateClintonFreeman = act (1, A) TheOvergrownEstateClintonFreeman Cards.theOvergrownEstateClintonFreeman Nothing

instance RunMessage TheOvergrownEstateClintonFreeman where
  runMessage msg a@(TheOvergrownEstateClintonFreeman attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheOvergrownEstateClintonFreeman <$> liftRunMessage msg attrs
