module Arkham.Treachery.Cards.ElderMist (elderMist) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ElderMist = ElderMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderMist :: TreacheryCard ElderMist
elderMist = treachery ElderMist Cards.elderMist

-- TODO: abilities
instance RunMessage ElderMist where
  runMessage msg (ElderMist attrs) = runQueueT $ ElderMist <$> liftRunMessage msg attrs
