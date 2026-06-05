module Arkham.Treachery.Cards.DevouringOoze (devouringOoze) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DevouringOoze = DevouringOoze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devouringOoze :: TreacheryCard DevouringOoze
devouringOoze = treachery DevouringOoze Cards.devouringOoze

instance RunMessage DevouringOoze where
  runMessage msg (DevouringOoze attrs) = DevouringOoze <$> runMessage msg attrs
