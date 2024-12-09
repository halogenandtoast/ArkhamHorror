module Arkham.Location.Cards.ExhibitHallRestrictedHall (
  exhibitHallRestrictedHall,
  ExhibitHallRestrictedHall (..),
) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ExhibitHallRestrictedHall = ExhibitHallRestrictedHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exhibitHallRestrictedHall :: LocationCard ExhibitHallRestrictedHall
exhibitHallRestrictedHall = location ExhibitHallRestrictedHall Cards.exhibitHallRestrictedHall 3 (PerPlayer 2)

instance HasModifiersFor ExhibitHallRestrictedHall where
  getModifiersFor (ExhibitHallRestrictedHall a) = whenRevealed a do
    modifySelfWhenM a (selectAny $ enemyIs Cards.huntingHorror <> at_ (be a)) [CannotInvestigate]

instance RunMessage ExhibitHallRestrictedHall where
  runMessage msg (ExhibitHallRestrictedHall attrs) =
    ExhibitHallRestrictedHall <$> runMessage msg attrs
