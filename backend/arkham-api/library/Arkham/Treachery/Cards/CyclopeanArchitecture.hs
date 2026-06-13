module Arkham.Treachery.Cards.CyclopeanArchitecture (cyclopeanArchitecture) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CyclopeanArchitecture = CyclopeanArchitecture TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyclopeanArchitecture :: TreacheryCard CyclopeanArchitecture
cyclopeanArchitecture = treachery CyclopeanArchitecture Cards.cyclopeanArchitecture

-- TODO: abilities
instance RunMessage CyclopeanArchitecture where
  runMessage msg (CyclopeanArchitecture attrs) = runQueueT $ CyclopeanArchitecture <$> liftRunMessage msg attrs
