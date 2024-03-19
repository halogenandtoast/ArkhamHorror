module Arkham.Treachery.Cards.RestlessJourneyHardship
  ( restlessJourneyHardship
  , RestlessJourneyHardship(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RestlessJourneyHardship = RestlessJourneyHardship TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyHardship :: TreacheryCard RestlessJourneyHardship
restlessJourneyHardship = treachery RestlessJourneyHardship Cards.restlessJourneyHardship

instance RunMessage RestlessJourneyHardship where
  runMessage msg t@(RestlessJourneyHardship attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> RestlessJourneyHardship <$> lift (runMessage msg attrs)
