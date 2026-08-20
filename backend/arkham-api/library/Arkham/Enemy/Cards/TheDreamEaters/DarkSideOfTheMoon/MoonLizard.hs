module Arkham.Enemy.Cards.TheDreamEaters.DarkSideOfTheMoon.MoonLizard (moonLizard) where

import Arkham.Classes
import Arkham.Enemy.CardDefs.TheDreamEaters.DarkSideOfTheMoon qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Modifiers qualified as Mod
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.TheDreamEaters.DarkSideOfTheMoon.Helpers
import Arkham.Trait (Trait (Cave))

newtype MoonLizard = MoonLizard EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonLizard :: EnemyCard MoonLizard
moonLizard = enemy MoonLizard Cards.moonLizard

instance HasModifiersFor MoonLizard where
  getModifiersFor (MoonLizard attrs) = do
    mInvestigator <- selectOne $ investigatorEngagedWith attrs
    x <- maybe (pure 5) getAlarmLevel mInvestigator
    nonCaves <- select $ not_ (LocationWithTrait Cave)
    modifySelf attrs $ [Mod.EnemyFight x, Mod.EnemyEvade x] <> map CannotEnter nonCaves

instance RunMessage MoonLizard where
  runMessage msg (MoonLizard attrs) =
    MoonLizard <$> runMessage msg attrs
