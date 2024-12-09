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
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

laboringGug :: EnemyCard LaboringGug
laboringGug = enemy LaboringGug Cards.laboringGug (5, Static 5, 2) (3, 1)

instance HasModifiersFor LaboringGug where
  getModifiersFor (LaboringGug attrs) = do
    modifySelect
      attrs
      (locationIs Locations.theEnchantedPath)
      [CannotBeEnteredBy $ EnemyWithId attrs.id]

instance RunMessage LaboringGug where
  runMessage msg (LaboringGug attrs) =
    LaboringGug <$> runMessage msg attrs
