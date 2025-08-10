module Arkham.Treachery.Cards.BleedingWallsTheMidwinterGala (bleedingWallsTheMidwinterGala) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.BleedingWalls
import Arkham.Treachery.Import.Lifted

newtype BleedingWallsTheMidwinterGala = BleedingWallsTheMidwinterGala BleedingWalls
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

bleedingWallsTheMidwinterGala :: TreacheryCard BleedingWallsTheMidwinterGala
bleedingWallsTheMidwinterGala = treachery (BleedingWallsTheMidwinterGala . BleedingWalls) Cards.bleedingWallsTheMidwinterGala

instance RunMessage BleedingWallsTheMidwinterGala where
  runMessage msg (BleedingWallsTheMidwinterGala inner) =
    BleedingWallsTheMidwinterGala <$> runMessage msg inner
