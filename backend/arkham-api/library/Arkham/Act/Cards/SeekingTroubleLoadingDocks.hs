module Arkham.Act.Cards.SeekingTroubleLoadingDocks (seekingTroubleLoadingDocks) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SeekingTroubleLoadingDocks = SeekingTroubleLoadingDocks ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

seekingTroubleLoadingDocks :: ActCard SeekingTroubleLoadingDocks
seekingTroubleLoadingDocks = act (1, A) SeekingTroubleLoadingDocks Cards.seekingTroubleLoadingDocks Nothing

instance RunMessage SeekingTroubleLoadingDocks where
  runMessage msg a@(SeekingTroubleLoadingDocks attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SeekingTroubleLoadingDocks <$> liftRunMessage msg attrs
