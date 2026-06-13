module Arkham.Treachery.Cards.DeadlyMechanisms (deadlyMechanisms) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeadlyMechanisms = DeadlyMechanisms TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlyMechanisms :: TreacheryCard DeadlyMechanisms
deadlyMechanisms = treachery DeadlyMechanisms Cards.deadlyMechanisms

-- TODO: abilities
instance RunMessage DeadlyMechanisms where
  runMessage msg (DeadlyMechanisms attrs) = runQueueT $ DeadlyMechanisms <$> liftRunMessage msg attrs
