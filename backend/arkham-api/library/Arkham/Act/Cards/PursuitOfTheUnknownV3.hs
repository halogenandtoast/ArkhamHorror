module Arkham.Act.Cards.PursuitOfTheUnknownV3 (pursuitOfTheUnknownV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype PursuitOfTheUnknownV3 = PursuitOfTheUnknownV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pursuitOfTheUnknownV3 :: ActCard PursuitOfTheUnknownV3
pursuitOfTheUnknownV3 = act (2, A) PursuitOfTheUnknownV3 Cards.pursuitOfTheUnknownV3 Nothing

instance RunMessage PursuitOfTheUnknownV3 where
  runMessage msg a@(PursuitOfTheUnknownV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PursuitOfTheUnknownV3 <$> liftRunMessage msg attrs
