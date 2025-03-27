module Arkham.Act.Cards.PursuitOfTheUnknownV2 (pursuitOfTheUnknownV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype PursuitOfTheUnknownV2 = PursuitOfTheUnknownV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pursuitOfTheUnknownV2 :: ActCard PursuitOfTheUnknownV2
pursuitOfTheUnknownV2 = act (2, A) PursuitOfTheUnknownV2 Cards.pursuitOfTheUnknownV2 Nothing

instance RunMessage PursuitOfTheUnknownV2 where
  runMessage msg a@(PursuitOfTheUnknownV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> PursuitOfTheUnknownV2 <$> liftRunMessage msg attrs
