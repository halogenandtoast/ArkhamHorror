module Arkham.Act.Cards.ThroughTheLabyrinth
  ( ThroughTheLabyrinth(..)
  , throughTheLabyrinth
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ThroughTheLabyrinth = ThroughTheLabyrinth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

throughTheLabyrinth :: ActCard ThroughTheLabyrinth
throughTheLabyrinth = act (1, A) ThroughTheLabyrinth Cards.throughTheLabyrinth Nothing

instance RunMessage ThroughTheLabyrinth where
  runMessage msg a@(ThroughTheLabyrinth attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ThroughTheLabyrinth <$> liftRunMessage msg attrs
