module Arkham.Enemy.Cards.LaboringGug (
  laboringGug,
  LaboringGug (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype LaboringGug = LaboringGug EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

laboringGug :: EnemyCard LaboringGug
laboringGug = enemy LaboringGug Cards.laboringGug (5, Static 5, 2) (3, 1)

instance HasModifiersFor LaboringGug where
  getModifiersFor (LocationTarget lid) (LaboringGug attrs) = do
    isTheEnchantedPath <- lid <=~> locationIs Locations.theEnchantedPath
    pure $ toModifiers attrs [CannotBeEnteredBy $ EnemyWithId $ toId attrs | isTheEnchantedPath]
  getModifiersFor _ _ = pure []

instance RunMessage LaboringGug where
  runMessage msg (LaboringGug attrs) =
    LaboringGug <$> runMessage msg attrs
