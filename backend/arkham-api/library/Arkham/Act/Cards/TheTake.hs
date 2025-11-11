module Arkham.Act.Cards.TheTake (theTake) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheTake = TheTake ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theTake :: ActCard TheTake
theTake = act (2, A) TheTake Cards.theTake Nothing

instance RunMessage TheTake where
  runMessage msg a@(TheTake attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheTake <$> liftRunMessage msg attrs
