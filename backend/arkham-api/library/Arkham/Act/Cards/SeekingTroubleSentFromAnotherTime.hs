module Arkham.Act.Cards.SeekingTroubleSentFromAnotherTime (seekingTroubleSentFromAnotherTime) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SeekingTroubleSentFromAnotherTime = SeekingTroubleSentFromAnotherTime ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

seekingTroubleSentFromAnotherTime :: ActCard SeekingTroubleSentFromAnotherTime
seekingTroubleSentFromAnotherTime = act (1, A) SeekingTroubleSentFromAnotherTime Cards.seekingTroubleSentFromAnotherTime Nothing

instance RunMessage SeekingTroubleSentFromAnotherTime where
  runMessage msg a@(SeekingTroubleSentFromAnotherTime attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SeekingTroubleSentFromAnotherTime <$> liftRunMessage msg attrs
