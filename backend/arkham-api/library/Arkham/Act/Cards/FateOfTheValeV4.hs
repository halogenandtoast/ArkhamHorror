module Arkham.Act.Cards.FateOfTheValeV4 (fateOfTheValeV4) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FateOfTheValeV4 = FateOfTheValeV4 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV4 :: ActCard FateOfTheValeV4
fateOfTheValeV4 = act (3, A) FateOfTheValeV4 Cards.fateOfTheValeV4 Nothing

instance RunMessage FateOfTheValeV4 where
  runMessage msg (FateOfTheValeV4 attrs) =
    runQueueT $ FateOfTheValeV4 <$> liftRunMessage msg attrs
