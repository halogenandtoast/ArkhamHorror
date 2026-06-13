module Arkham.Treachery.Cards.EyesOfYchlecht (eyesOfYchlecht) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EyesOfYchlecht = EyesOfYchlecht TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesOfYchlecht :: TreacheryCard EyesOfYchlecht
eyesOfYchlecht = treachery EyesOfYchlecht Cards.eyesOfYchlecht

-- TODO: abilities
instance RunMessage EyesOfYchlecht where
  runMessage msg (EyesOfYchlecht attrs) = runQueueT $ EyesOfYchlecht <$> liftRunMessage msg attrs
