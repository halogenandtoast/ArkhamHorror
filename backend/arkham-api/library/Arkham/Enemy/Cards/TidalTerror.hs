module Arkham.Enemy.Cards.TidalTerror (tidalTerror) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype TidalTerror = TidalTerror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tidalTerror :: EnemyCard TidalTerror
tidalTerror =
  enemy TidalTerror Cards.tidalTerror (4, Static 4, 2) (1, 2)
    & setSpawnAt (oneOf [locationIs Locations.porteDeLAvancee, "Chapel of St. Aubert"])

instance RunMessage TidalTerror where
  runMessage msg (TidalTerror attrs) = TidalTerror <$> runMessage msg attrs
