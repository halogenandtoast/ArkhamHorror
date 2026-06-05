module Arkham.Treachery.Cards.CorrosiveSlime (corrosiveSlime) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CorrosiveSlime = CorrosiveSlime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosiveSlime :: TreacheryCard CorrosiveSlime
corrosiveSlime = treachery CorrosiveSlime Cards.corrosiveSlime

instance RunMessage CorrosiveSlime where
  runMessage msg (CorrosiveSlime attrs) = CorrosiveSlime <$> runMessage msg attrs
