module Arkham.Act.Cards.DawnOfTheFinalDay (dawnOfTheFinalDay) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype DawnOfTheFinalDay = DawnOfTheFinalDay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dawnOfTheFinalDay :: ActCard DawnOfTheFinalDay
dawnOfTheFinalDay = act (1, A) DawnOfTheFinalDay Cards.dawnOfTheFinalDay Nothing

instance RunMessage DawnOfTheFinalDay where
  runMessage msg (DawnOfTheFinalDay attrs) = runQueueT $ case msg of
    _ -> DawnOfTheFinalDay <$> liftRunMessage msg attrs
