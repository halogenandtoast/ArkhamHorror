module Arkham.Homebrew.AgesUnwound.Acts.OutOfYourDepth (outOfYourDepth) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards

newtype OutOfYourDepth = OutOfYourDepth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

outOfYourDepth :: ActCard OutOfYourDepth
outOfYourDepth = act (1, A) OutOfYourDepth Cards.outOfYourDepth (groupClueCost $ PerPlayer 3)

instance RunMessage OutOfYourDepth where
  runMessage msg a@(OutOfYourDepth attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> OutOfYourDepth <$> liftRunMessage msg attrs
