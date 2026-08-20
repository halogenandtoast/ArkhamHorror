module Arkham.Treachery.Cards.TheMidwinterGala.BleedingWalls (bleedingWalls) where

import Arkham.Treachery.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Treachery.Cards.ThePathToCarcosa.DecayingReality.BleedingWalls qualified as Base
import Arkham.Treachery.Import.Lifted

newtype BleedingWalls = BleedingWalls Base.BleedingWalls
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

bleedingWalls :: TreacheryCard BleedingWalls
bleedingWalls =
  treachery
    (BleedingWalls . Base.BleedingWalls)
    Cards.bleedingWalls

instance RunMessage BleedingWalls where
  runMessage msg (BleedingWalls inner) =
    BleedingWalls <$> runMessage msg inner
