module Arkham.Enemy.Cards.TheDreamEaters.BeyondTheGatesOfSleep.LaboringGug (laboringGug) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.BeyondTheGatesOfSleep qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.TheDreamEaters.BeyondTheGatesOfSleep qualified as Locations
import Arkham.Matcher
import Arkham.Prelude

newtype LaboringGug = LaboringGug EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

laboringGug :: EnemyCard LaboringGug
laboringGug = enemy LaboringGug Cards.laboringGug

instance HasModifiersFor LaboringGug where
  getModifiersFor (LaboringGug attrs) = do
    modifySelect
      attrs
      (locationIs Locations.theEnchantedPath)
      [CannotBeEnteredBy $ EnemyWithId attrs.id]

instance RunMessage LaboringGug where
  runMessage msg (LaboringGug attrs) =
    runQueueT
      $ LaboringGug
      <$> liftRunMessage msg attrs
