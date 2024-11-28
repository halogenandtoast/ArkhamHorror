module Arkham.Act.Cards.TheLostExpedition
  ( TheLostExpedition(..)
  , theLostExpedition
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheLostExpedition = TheLostExpedition ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theLostExpedition :: ActCard TheLostExpedition
theLostExpedition = act (1, A) TheLostExpedition Cards.theLostExpedition Nothing

instance RunMessage TheLostExpedition where
  runMessage msg a@(TheLostExpedition attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheLostExpedition <$> liftRunMessage msg attrs
