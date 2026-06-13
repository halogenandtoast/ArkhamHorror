module Arkham.Treachery.Cards.DangerousCuriosity (dangerousCuriosity) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DangerousCuriosity = DangerousCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dangerousCuriosity :: TreacheryCard DangerousCuriosity
dangerousCuriosity = treachery DangerousCuriosity Cards.dangerousCuriosity

-- TODO: abilities
instance RunMessage DangerousCuriosity where
  runMessage msg (DangerousCuriosity attrs) = runQueueT $ DangerousCuriosity <$> liftRunMessage msg attrs
