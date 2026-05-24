module Arkham.Act.Cards.OnTheTrail (onTheTrail) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype OnTheTrail = OnTheTrail ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

onTheTrail :: ActCard OnTheTrail
onTheTrail = act (2, A) OnTheTrail Cards.onTheTrail Nothing

instance RunMessage OnTheTrail where
  runMessage msg a@(OnTheTrail attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> OnTheTrail <$> liftRunMessage msg attrs
