module Arkham.Location.Cards.ExhibitHallRestrictedHall
  ( exhibitHallRestrictedHall
  , ExhibitHallRestrictedHall(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Modifier

newtype ExhibitHallRestrictedHall = ExhibitHallRestrictedHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

exhibitHallRestrictedHall :: LocationCard ExhibitHallRestrictedHall
exhibitHallRestrictedHall = locationWithRevealedSideConnections
  ExhibitHallRestrictedHall
  Cards.exhibitHallRestrictedHall
  3
  (PerPlayer 2)
  NoSymbol
  [Square]
  Equals
  [Square]

instance Query EnemyMatcher env => HasModifiersFor env ExhibitHallRestrictedHall where
  getModifiersFor _ target (ExhibitHallRestrictedHall attrs)
    | isTarget attrs target = do
      mHuntingHorror <- selectOne $ enemyIs Cards.huntingHorror <> EnemyAt
        (LocationWithId $ toId attrs)
      pure $ toModifiers attrs [ CannotInvestigate | isJust mHuntingHorror ]
  getModifiersFor _ _ _ = pure []

instance LocationRunner env => RunMessage ExhibitHallRestrictedHall where
  runMessage msg (ExhibitHallRestrictedHall attrs) =
    ExhibitHallRestrictedHall <$> runMessage msg attrs
