module Arkham.Enemy.Cards.MoonLizard (moonLizard, MoonLizard (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers qualified as Mod
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Trait (Trait (Cave))

newtype MoonLizard = MoonLizard EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonLizard :: EnemyCard MoonLizard
moonLizard = enemy MoonLizard Cards.moonLizard (0, PerPlayer 4, 0) (2, 2)

instance HasModifiersFor MoonLizard where
  getModifiersFor target (MoonLizard attrs) | attrs `is` target = do
    mInvestigator <- selectOne $ investigatorEngagedWith attrs
    x <- maybe (pure 5) getAlarmLevel mInvestigator
    nonCaves <- select $ not_ (LocationWithTrait Cave)
    pure $ toModifiers attrs $ [Mod.EnemyFight x, Mod.EnemyEvade x] <> map CannotEnter nonCaves
  getModifiersFor _ _ = pure []

instance RunMessage MoonLizard where
  runMessage msg (MoonLizard attrs) =
    MoonLizard <$> runMessage msg attrs
