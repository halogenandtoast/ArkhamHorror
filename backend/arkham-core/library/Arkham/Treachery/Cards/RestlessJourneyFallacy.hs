module Arkham.Treachery.Cards.RestlessJourneyFallacy (
  restlessJourneyFallacy,
  RestlessJourneyFallacy (..),
)
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RestlessJourneyFallacy = RestlessJourneyFallacy TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restlessJourneyFallacy :: TreacheryCard RestlessJourneyFallacy
restlessJourneyFallacy = treachery RestlessJourneyFallacy Cards.restlessJourneyFallacy

instance RunMessage RestlessJourneyFallacy where
  runMessage msg t@(RestlessJourneyFallacy attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> RestlessJourneyFallacy <$> lift (runMessage msg attrs)
