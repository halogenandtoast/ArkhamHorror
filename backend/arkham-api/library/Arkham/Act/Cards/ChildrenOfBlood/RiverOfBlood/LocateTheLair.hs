module Arkham.Act.Cards.ChildrenOfBlood.RiverOfBlood.LocateTheLair (locateTheLair) where

import Arkham.Act.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Act.Import.Lifted

newtype LocateTheLair = LocateTheLair ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

locateTheLair :: ActCard LocateTheLair
locateTheLair = act (1, A) LocateTheLair Cards.locateTheLair Nothing

instance RunMessage LocateTheLair where
  runMessage msg a@(LocateTheLair attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> LocateTheLair <$> liftRunMessage msg attrs
