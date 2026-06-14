module Arkham.Location.Cards.AcidicCoelom (acidicCoelom) where

import Arkham.Cost
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AcidicCoelom = AcidicCoelom LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acidicCoelom :: LocationCard AcidicCoelom
acidicCoelom = location AcidicCoelom Cards.acidicCoelom 4 (Static 1)

instance HasModifiersFor AcidicCoelom where
  getModifiersFor (AcidicCoelom a) =
    modifySelect
      a
      (investigator_ $ at_ (be a))
      [ AdditionalCostToEnterMatching
          (LocationWithTitle "Apiary Entrance")
          (DamageCost (toSource a) YouTarget 1)
      ]

instance RunMessage AcidicCoelom where
  runMessage msg (AcidicCoelom attrs) = runQueueT $ AcidicCoelom <$> liftRunMessage msg attrs
