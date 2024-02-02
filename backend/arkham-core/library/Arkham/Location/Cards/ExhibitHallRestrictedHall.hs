module Arkham.Location.Cards.ExhibitHallRestrictedHall (
  exhibitHallRestrictedHall,
  ExhibitHallRestrictedHall (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype ExhibitHallRestrictedHall = ExhibitHallRestrictedHall LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

exhibitHallRestrictedHall :: LocationCard ExhibitHallRestrictedHall
exhibitHallRestrictedHall =
  location
    ExhibitHallRestrictedHall
    Cards.exhibitHallRestrictedHall
    3
    (PerPlayer 2)

instance HasModifiersFor ExhibitHallRestrictedHall where
  getModifiersFor target (ExhibitHallRestrictedHall attrs)
    | isTarget attrs target = do
        mHuntingHorror <-
          selectOne
            $ enemyIs Cards.huntingHorror
            <> EnemyAt
              (LocationWithId $ toId attrs)
        pure $ toModifiers attrs [CannotInvestigate | isJust mHuntingHorror]
  getModifiersFor _ _ = pure []

instance RunMessage ExhibitHallRestrictedHall where
  runMessage msg (ExhibitHallRestrictedHall attrs) =
    ExhibitHallRestrictedHall <$> runMessage msg attrs
