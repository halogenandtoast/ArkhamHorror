module Arkham.Location.Cards.CoreOfTheVaultHeartOfTheMachine (coreOfTheVaultHeartOfTheMachine) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CoreOfTheVaultHeartOfTheMachine = CoreOfTheVaultHeartOfTheMachine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coreOfTheVaultHeartOfTheMachine :: LocationCard CoreOfTheVaultHeartOfTheMachine
coreOfTheVaultHeartOfTheMachine = location CoreOfTheVaultHeartOfTheMachine Cards.coreOfTheVaultHeartOfTheMachine 3 (Static 3)

-- TODO: abilities

instance RunMessage CoreOfTheVaultHeartOfTheMachine where
  runMessage msg (CoreOfTheVaultHeartOfTheMachine attrs) = runQueueT $ CoreOfTheVaultHeartOfTheMachine <$> liftRunMessage msg attrs
