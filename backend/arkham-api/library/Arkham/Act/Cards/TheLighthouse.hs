module Arkham.Act.Cards.TheLighthouse
  ( TheLighthouse(..)
  , theLighthouse
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheLighthouse = TheLighthouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theLighthouse :: ActCard TheLighthouse
theLighthouse = act (1, A) TheLighthouse Cards.theLighthouse Nothing

instance RunMessage TheLighthouse where
  runMessage msg a@(TheLighthouse attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheLighthouse <$> liftRunMessage msg attrs
