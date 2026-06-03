module Arkham.Act.Cards.FateOfTheValeV1 (fateOfTheValeV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FateOfTheValeV1 = FateOfTheValeV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV1 :: ActCard FateOfTheValeV1
fateOfTheValeV1 = act (3, A) FateOfTheValeV1 Cards.fateOfTheValeV1 Nothing

instance RunMessage FateOfTheValeV1 where
  runMessage msg (FateOfTheValeV1 attrs) =
    runQueueT $ FateOfTheValeV1 <$> liftRunMessage msg attrs
