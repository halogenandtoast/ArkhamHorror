module Arkham.Act.Cards.ChildrenOfBlood.RiverOfBlood.CornerTheSuspect (cornerTheSuspect) where

import Arkham.Act.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Act.Import.Lifted

newtype CornerTheSuspect = CornerTheSuspect ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cornerTheSuspect :: ActCard CornerTheSuspect
cornerTheSuspect = act (2, A) CornerTheSuspect Cards.cornerTheSuspect Nothing

instance RunMessage CornerTheSuspect where
  runMessage msg a@(CornerTheSuspect attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CornerTheSuspect <$> liftRunMessage msg attrs
