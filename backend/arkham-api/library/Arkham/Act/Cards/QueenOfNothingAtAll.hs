module Arkham.Act.Cards.QueenOfNothingAtAll (queenOfNothingAtAll) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype QueenOfNothingAtAll = QueenOfNothingAtAll ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

queenOfNothingAtAll :: ActCard QueenOfNothingAtAll
queenOfNothingAtAll = act (3, A) QueenOfNothingAtAll Cards.queenOfNothingAtAll Nothing

instance RunMessage QueenOfNothingAtAll where
  runMessage msg a@(QueenOfNothingAtAll attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> QueenOfNothingAtAll <$> liftRunMessage msg attrs
