module Arkham.Treachery.Cards.ObsessedGamblerC (obsessedGamblerC) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ObsessedGamblerC = ObsessedGamblerC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsessedGamblerC :: TreacheryCard ObsessedGamblerC
obsessedGamblerC = treachery ObsessedGamblerC Cards.obsessedGamblerC

instance RunMessage ObsessedGamblerC where
  runMessage msg t@(ObsessedGamblerC attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ObsessedGamblerC <$> liftRunMessage msg attrs
