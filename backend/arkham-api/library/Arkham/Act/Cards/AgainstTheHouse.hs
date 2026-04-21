module Arkham.Act.Cards.AgainstTheHouse (againstTheHouse) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype AgainstTheHouse = AgainstTheHouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

againstTheHouse :: ActCard AgainstTheHouse
againstTheHouse = act (2, A) AgainstTheHouse Cards.againstTheHouse Nothing

instance RunMessage AgainstTheHouse where
  runMessage msg a@(AgainstTheHouse attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> AgainstTheHouse <$> liftRunMessage msg attrs
