module Arkham.Treachery.Cards.EyeOfTheDeep (eyeOfTheDeep) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EyeOfTheDeep = EyeOfTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfTheDeep :: TreacheryCard EyeOfTheDeep
eyeOfTheDeep = treachery EyeOfTheDeep Cards.eyeOfTheDeep

-- TODO: abilities
instance RunMessage EyeOfTheDeep where
  runMessage msg (EyeOfTheDeep attrs) = runQueueT $ EyeOfTheDeep <$> liftRunMessage msg attrs
