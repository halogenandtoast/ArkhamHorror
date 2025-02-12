module Arkham.Treachery.Cards.ProphecyOfTheEnd (prophecyOfTheEnd) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

-- N.B. This card is handled on Gloria herself

newtype ProphecyOfTheEnd = ProphecyOfTheEnd TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prophecyOfTheEnd :: TreacheryCard ProphecyOfTheEnd
prophecyOfTheEnd = treachery ProphecyOfTheEnd Cards.prophecyOfTheEnd

instance RunMessage ProphecyOfTheEnd where
  runMessage msg (ProphecyOfTheEnd attrs) = 
    ProphecyOfTheEnd <$> runMessage msg attrs
