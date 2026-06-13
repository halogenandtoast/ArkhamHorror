module Arkham.Treachery.Cards.Domination (domination) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Domination = Domination TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

domination :: TreacheryCard Domination
domination = treachery Domination Cards.domination

-- TODO: abilities
instance RunMessage Domination where
  runMessage msg (Domination attrs) = runQueueT $ Domination <$> liftRunMessage msg attrs
