module Arkham.Treachery.Cards.ObsessedGamblerA (obsessedGamblerA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ObsessedGamblerA = ObsessedGamblerA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsessedGamblerA :: TreacheryCard ObsessedGamblerA
obsessedGamblerA = treachery ObsessedGamblerA Cards.obsessedGamblerA

instance RunMessage ObsessedGamblerA where
  runMessage msg t@(ObsessedGamblerA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ObsessedGamblerA <$> liftRunMessage msg attrs
