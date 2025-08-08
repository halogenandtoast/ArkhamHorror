module Arkham.Treachery.Cards.NoxiousFumesTheMidwinterGala (noxiousFumesTheMidwinterGala) where

import Arkham.Treachery.Cards.NoxiousFumes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NoxiousFumesTheMidwinterGala = NoxiousFumesTheMidwinterGala NoxiousFumes
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities, HasModifiersFor)

noxiousFumesTheMidwinterGala :: TreacheryCard NoxiousFumesTheMidwinterGala
noxiousFumesTheMidwinterGala = treachery (NoxiousFumesTheMidwinterGala . NoxiousFumes) Cards.noxiousFumesTheMidwinterGala

instance RunMessage NoxiousFumesTheMidwinterGala where
  runMessage msg (NoxiousFumesTheMidwinterGala inner) =
    NoxiousFumesTheMidwinterGala <$> runMessage msg inner
