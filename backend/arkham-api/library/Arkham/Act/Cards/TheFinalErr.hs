module Arkham.Act.Cards.TheFinalErr (theFinalErr) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheFinalErr = TheFinalErr ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theFinalErr :: ActCard TheFinalErr
theFinalErr = act (4, A) TheFinalErr Cards.theFinalErr Nothing

instance RunMessage TheFinalErr where
  runMessage msg a@(TheFinalErr attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheFinalErr <$> liftRunMessage msg attrs
