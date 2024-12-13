module Arkham.Act.Cards.UnderAttack (underAttack) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype UnderAttack = UnderAttack ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

underAttack :: ActCard UnderAttack
underAttack = act (3, A) UnderAttack Cards.underAttack Nothing

instance RunMessage UnderAttack where
  runMessage msg a@(UnderAttack attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> UnderAttack <$> liftRunMessage msg attrs
