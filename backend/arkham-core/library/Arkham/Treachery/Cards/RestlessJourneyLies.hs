module Arkham.Treachery.Cards.RestlessJourneyLies
  ( restlessJourneyLies
  , RestlessJourneyLies(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RestlessJourneyLies = RestlessJourneyLies TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyLies :: TreacheryCard RestlessJourneyLies
restlessJourneyLies = treachery RestlessJourneyLies Cards.restlessJourneyLies

instance RunMessage RestlessJourneyLies where
  runMessage msg t@(RestlessJourneyLies attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> RestlessJourneyLies <$> lift (runMessage msg attrs)
