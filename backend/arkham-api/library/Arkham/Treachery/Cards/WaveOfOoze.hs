module Arkham.Treachery.Cards.WaveOfOoze (waveOfOoze) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WaveOfOoze = WaveOfOoze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waveOfOoze :: TreacheryCard WaveOfOoze
waveOfOoze = treachery WaveOfOoze Cards.waveOfOoze

instance RunMessage WaveOfOoze where
  runMessage msg (WaveOfOoze attrs) = WaveOfOoze <$> runMessage msg attrs
