module Arkham.Treachery.Cards.ObsessedGamblerB (obsessedGamblerB) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ObsessedGamblerB = ObsessedGamblerB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsessedGamblerB :: TreacheryCard ObsessedGamblerB
obsessedGamblerB = treachery ObsessedGamblerB Cards.obsessedGamblerB

instance RunMessage ObsessedGamblerB where
  runMessage msg t@(ObsessedGamblerB attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ObsessedGamblerB <$> liftRunMessage msg attrs
