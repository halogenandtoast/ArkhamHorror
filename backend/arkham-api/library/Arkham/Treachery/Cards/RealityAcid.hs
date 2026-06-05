module Arkham.Treachery.Cards.RealityAcid (realityAcid) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RealityAcid = RealityAcid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcid :: TreacheryCard RealityAcid
realityAcid = treachery RealityAcid Cards.realityAcid

instance RunMessage RealityAcid where
  runMessage msg (RealityAcid attrs) = RealityAcid <$> runMessage msg attrs
