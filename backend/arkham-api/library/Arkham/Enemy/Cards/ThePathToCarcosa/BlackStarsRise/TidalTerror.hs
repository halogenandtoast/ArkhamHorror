module Arkham.Enemy.Cards.ThePathToCarcosa.BlackStarsRise.TidalTerror (tidalTerror) where

import Arkham.Enemy.CardDefs.ThePathToCarcosa.BlackStarsRise qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Location.CardDefs.ThePathToCarcosa.BlackStarsRise qualified as Locations
import Arkham.Matcher

newtype TidalTerror = TidalTerror EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tidalTerror :: EnemyCard TidalTerror
tidalTerror =
  enemy TidalTerror Cards.tidalTerror
    & setSpawnAt (oneOf [locationIs Locations.porteDeLAvancee, "Chapel of St. Aubert"])

instance RunMessage TidalTerror where
  runMessage msg (TidalTerror attrs) = TidalTerror <$> runMessage msg attrs
