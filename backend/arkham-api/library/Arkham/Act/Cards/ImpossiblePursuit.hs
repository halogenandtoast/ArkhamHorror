module Arkham.Act.Cards.ImpossiblePursuit (impossiblePursuit) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ImpossiblePursuit = ImpossiblePursuit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

impossiblePursuit :: ActCard ImpossiblePursuit
impossiblePursuit = act (1, A) ImpossiblePursuit Cards.impossiblePursuit Nothing

instance RunMessage ImpossiblePursuit where
  runMessage msg a@(ImpossiblePursuit attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ImpossiblePursuit <$> liftRunMessage msg attrs
