module Arkham.Act.Cards.TraversingTheOutside (traversingTheOutside) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype TraversingTheOutside = TraversingTheOutside ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

traversingTheOutside :: ActCard TraversingTheOutside
traversingTheOutside = act (1, A) TraversingTheOutside Cards.traversingTheOutside Nothing

instance RunMessage TraversingTheOutside where
  runMessage msg a@(TraversingTheOutside attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TraversingTheOutside <$> liftRunMessage msg attrs
