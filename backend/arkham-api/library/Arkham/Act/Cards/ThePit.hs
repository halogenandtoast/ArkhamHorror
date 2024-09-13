module Arkham.Act.Cards.ThePit (
  ThePit (..),
  thePit,
) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ThePit = ThePit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thePit :: ActCard ThePit
thePit = act (1, A) ThePit Cards.thePit (groupClueCost $ Static 3)

instance RunMessage ThePit where
  runMessage msg a@(ThePit attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ThePit <$> liftRunMessage msg attrs
