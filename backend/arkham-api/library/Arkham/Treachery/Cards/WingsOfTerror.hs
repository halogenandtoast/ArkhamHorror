module Arkham.Treachery.Cards.WingsOfTerror (wingsOfTerror) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WingsOfTerror = WingsOfTerror TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingsOfTerror :: TreacheryCard WingsOfTerror
wingsOfTerror = treachery WingsOfTerror Cards.wingsOfTerror

-- TODO: abilities
instance RunMessage WingsOfTerror where
  runMessage msg (WingsOfTerror attrs) = runQueueT $ WingsOfTerror <$> liftRunMessage msg attrs
