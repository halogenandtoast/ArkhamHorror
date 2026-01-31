module Arkham.Act.Cards.DesperateSearch (desperateSearch) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype DesperateSearch = DesperateSearch ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

desperateSearch :: ActCard DesperateSearch
desperateSearch = act (1, A) DesperateSearch Cards.desperateSearch Nothing

instance RunMessage DesperateSearch where
  runMessage msg a@(DesperateSearch attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> DesperateSearch <$> liftRunMessage msg attrs
