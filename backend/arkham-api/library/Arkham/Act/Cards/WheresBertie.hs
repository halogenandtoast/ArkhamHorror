module Arkham.Act.Cards.WheresBertie (wheresBertie) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype WheresBertie = WheresBertie ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

wheresBertie :: ActCard WheresBertie
wheresBertie = act (2, A) WheresBertie Cards.wheresBertie Nothing

instance RunMessage WheresBertie where
  runMessage msg a@(WheresBertie attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> WheresBertie <$> liftRunMessage msg attrs
