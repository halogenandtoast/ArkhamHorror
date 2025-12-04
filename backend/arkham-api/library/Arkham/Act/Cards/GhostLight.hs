module Arkham.Act.Cards.GhostLight (ghostLight) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype GhostLight = GhostLight ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ghostLight :: ActCard GhostLight
ghostLight = act (1, A) GhostLight Cards.ghostLight Nothing

instance RunMessage GhostLight where
  runMessage msg a@(GhostLight attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> GhostLight <$> liftRunMessage msg attrs
