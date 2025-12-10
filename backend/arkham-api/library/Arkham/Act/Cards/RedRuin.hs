module Arkham.Act.Cards.RedRuin (redRuin) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype RedRuin = RedRuin ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

redRuin :: ActCard RedRuin
redRuin = act (2, A) RedRuin Cards.redRuin Nothing

instance RunMessage RedRuin where
  runMessage msg a@(RedRuin attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> RedRuin <$> liftRunMessage msg attrs
