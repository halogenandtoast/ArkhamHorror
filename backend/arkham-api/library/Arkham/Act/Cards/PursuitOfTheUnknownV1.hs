module Arkham.Act.Cards.PursuitOfTheUnknownV1 (pursuitOfTheUnknownV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype PursuitOfTheUnknownV1 = PursuitOfTheUnknownV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pursuitOfTheUnknownV1 :: ActCard PursuitOfTheUnknownV1
pursuitOfTheUnknownV1 = act (2, A) PursuitOfTheUnknownV1 Cards.pursuitOfTheUnknownV1 Nothing

instance RunMessage PursuitOfTheUnknownV1 where
  runMessage msg a@(PursuitOfTheUnknownV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PursuitOfTheUnknownV1 <$> liftRunMessage msg attrs
