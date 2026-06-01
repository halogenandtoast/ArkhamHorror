module Arkham.Act.Cards.FateOfTheValeV2 (fateOfTheValeV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype FateOfTheValeV2 = FateOfTheValeV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV2 :: ActCard FateOfTheValeV2
fateOfTheValeV2 = act (3, A) FateOfTheValeV2 Cards.fateOfTheValeV2 Nothing

instance RunMessage FateOfTheValeV2 where
  runMessage msg (FateOfTheValeV2 attrs) =
    runQueueT $ FateOfTheValeV2 <$> liftRunMessage msg attrs
