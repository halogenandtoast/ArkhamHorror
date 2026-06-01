module Arkham.Act.Cards.FateOfTheValeV3 (fateOfTheValeV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FateOfTheValeV3 = FateOfTheValeV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV3 :: ActCard FateOfTheValeV3
fateOfTheValeV3 = act (3, A) FateOfTheValeV3 Cards.fateOfTheValeV3 Nothing

instance RunMessage FateOfTheValeV3 where
  runMessage msg (FateOfTheValeV3 attrs) =
    runQueueT $ FateOfTheValeV3 <$> liftRunMessage msg attrs
