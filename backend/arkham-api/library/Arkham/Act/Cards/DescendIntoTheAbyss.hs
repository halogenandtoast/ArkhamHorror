module Arkham.Act.Cards.DescendIntoTheAbyss (descendIntoTheAbyss) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype DescendIntoTheAbyss = DescendIntoTheAbyss ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descendIntoTheAbyss :: ActCard DescendIntoTheAbyss
descendIntoTheAbyss = act (1, A) DescendIntoTheAbyss Cards.descendIntoTheAbyss Nothing

-- TODO: abilities
instance HasAbilities DescendIntoTheAbyss where
  getAbilities _ = []

instance RunMessage DescendIntoTheAbyss where
  runMessage msg (DescendIntoTheAbyss attrs) = runQueueT $ DescendIntoTheAbyss <$> liftRunMessage msg attrs
