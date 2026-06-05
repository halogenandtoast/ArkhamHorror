module Arkham.Treachery.Cards.CausticDissemination (causticDissemination) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CausticDissemination = CausticDissemination TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

causticDissemination :: TreacheryCard CausticDissemination
causticDissemination = treachery CausticDissemination Cards.causticDissemination

instance RunMessage CausticDissemination where
  runMessage msg (CausticDissemination attrs) = CausticDissemination <$> runMessage msg attrs
