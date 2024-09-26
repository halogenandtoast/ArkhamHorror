module Arkham.Treachery.Cards.PulledBack
  ( pulledBack
  , PulledBack(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PulledBack = PulledBack TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pulledBack :: TreacheryCard PulledBack
pulledBack = treachery PulledBack Cards.pulledBack

instance RunMessage PulledBack where
  runMessage msg t@(PulledBack attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> PulledBack <$> liftRunMessage msg attrs
