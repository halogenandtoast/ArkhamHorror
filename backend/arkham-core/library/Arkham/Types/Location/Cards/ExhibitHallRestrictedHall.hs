module Arkham.Types.Location.Cards.ExhibitHallRestrictedHall
  ( exhibitHallRestrictedHall
  , ExhibitHallRestrictedHall(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Modifier

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

instance LocationRunner env => RunMessage env ExhibitHallRestrictedHall where
  runMessage msg (ExhibitHallRestrictedHall attrs) =
    ExhibitHallRestrictedHall <$> runMessage msg attrs
