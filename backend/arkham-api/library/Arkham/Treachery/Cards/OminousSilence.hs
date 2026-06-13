module Arkham.Treachery.Cards.OminousSilence (ominousSilence) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OminousSilence = OminousSilence TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ominousSilence :: TreacheryCard OminousSilence
ominousSilence = treachery OminousSilence Cards.ominousSilence

-- TODO: abilities
instance RunMessage OminousSilence where
  runMessage msg (OminousSilence attrs) = runQueueT $ OminousSilence <$> liftRunMessage msg attrs
