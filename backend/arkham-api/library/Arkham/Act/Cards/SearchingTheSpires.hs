module Arkham.Act.Cards.SearchingTheSpires (searchingTheSpires) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SearchingTheSpires = SearchingTheSpires ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingTheSpires :: ActCard SearchingTheSpires
searchingTheSpires = act (1, A) SearchingTheSpires Cards.searchingTheSpires Nothing

-- TODO: abilities
instance HasAbilities SearchingTheSpires where
  getAbilities _ = []

instance RunMessage SearchingTheSpires where
  runMessage msg (SearchingTheSpires attrs) = runQueueT $ SearchingTheSpires <$> liftRunMessage msg attrs
