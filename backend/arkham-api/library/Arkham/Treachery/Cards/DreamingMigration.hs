module Arkham.Treachery.Cards.DreamingMigration (dreamingMigration) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DreamingMigration = DreamingMigration TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamingMigration :: TreacheryCard DreamingMigration
dreamingMigration = treachery DreamingMigration Cards.dreamingMigration

-- TODO: abilities
instance RunMessage DreamingMigration where
  runMessage msg (DreamingMigration attrs) = runQueueT $ DreamingMigration <$> liftRunMessage msg attrs
