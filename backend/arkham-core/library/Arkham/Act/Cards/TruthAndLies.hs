module Arkham.Act.Cards.TruthAndLies
  ( TruthAndLies(..)
  , truthAndLies
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TruthAndLies = TruthAndLies ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

truthAndLies :: ActCard TruthAndLies
truthAndLies = act (4, A) TruthAndLies Cards.truthAndLies Nothing

instance RunMessage TruthAndLies where
  runMessage msg a@(TruthAndLies attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TruthAndLies <$> lift (runMessage msg attrs)
