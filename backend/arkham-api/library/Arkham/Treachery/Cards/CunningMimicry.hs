module Arkham.Treachery.Cards.CunningMimicry (cunningMimicry) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CunningMimicry = CunningMimicry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cunningMimicry :: TreacheryCard CunningMimicry
cunningMimicry = treachery CunningMimicry Cards.cunningMimicry

-- TODO: abilities
instance RunMessage CunningMimicry where
  runMessage msg (CunningMimicry attrs) = runQueueT $ CunningMimicry <$> liftRunMessage msg attrs
