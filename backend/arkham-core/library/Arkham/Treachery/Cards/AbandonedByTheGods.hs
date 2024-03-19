module Arkham.Treachery.Cards.AbandonedByTheGods
  ( abandonedByTheGods
  , AbandonedByTheGods(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AbandonedByTheGods = AbandonedByTheGods TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedByTheGods :: TreacheryCard AbandonedByTheGods
abandonedByTheGods = treachery AbandonedByTheGods Cards.abandonedByTheGods

instance RunMessage AbandonedByTheGods where
  runMessage msg t@(AbandonedByTheGods attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> AbandonedByTheGods <$> lift (runMessage msg attrs)
