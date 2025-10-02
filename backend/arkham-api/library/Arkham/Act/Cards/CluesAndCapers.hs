module Arkham.Act.Cards.CluesAndCapers (cluesAndCapers) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CluesAndCapers = CluesAndCapers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cluesAndCapers :: ActCard CluesAndCapers
cluesAndCapers = act (1, A) CluesAndCapers Cards.cluesAndCapers Nothing

instance RunMessage CluesAndCapers where
  runMessage msg a@(CluesAndCapers attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CluesAndCapers <$> liftRunMessage msg attrs
