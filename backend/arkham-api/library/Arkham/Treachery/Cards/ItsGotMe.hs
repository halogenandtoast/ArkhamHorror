module Arkham.Treachery.Cards.ItsGotMe (itsGotMe) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ItsGotMe = ItsGotMe TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

itsGotMe :: TreacheryCard ItsGotMe
itsGotMe = treachery ItsGotMe Cards.itsGotMe

instance RunMessage ItsGotMe where
  runMessage msg (ItsGotMe attrs) = ItsGotMe <$> runMessage msg attrs
