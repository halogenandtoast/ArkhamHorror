module Arkham.Act.Cards.TheMissingSibling (theMissingSibling) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheMissingSibling = TheMissingSibling ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theMissingSibling :: ActCard TheMissingSibling
theMissingSibling = act (1, A) TheMissingSibling Cards.theMissingSibling Nothing

instance RunMessage TheMissingSibling where
  runMessage msg a@(TheMissingSibling attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheMissingSibling <$> liftRunMessage msg attrs
