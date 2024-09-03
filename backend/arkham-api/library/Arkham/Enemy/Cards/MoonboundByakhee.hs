module Arkham.Enemy.Cards.MoonboundByakhee (moonboundByakhee, MoonboundByakhee (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Token
import Arkham.Trait (Trait (Surface))

newtype MoonboundByakhee = MoonboundByakhee EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor MoonboundByakhee where
  getModifiersFor (InvestigatorTarget iid) (MoonboundByakhee attrs) = do
    x <- getAlarmLevel iid
    pure
      $ toModifiers attrs
      $ guard (x <= 2)
      *> [CannotBeEngagedBy (be attrs), CannotBeHuntedBy (be attrs)]
  getModifiersFor _ _ = pure []

moonboundByakhee :: EnemyCard MoonboundByakhee
moonboundByakhee =
  enemyWith MoonboundByakhee Cards.moonboundByakhee (3, Static 3, 3) (3, 1)
    $ ( spawnAtL
          ?~ SpawnAt (NearestLocationToYou $ LocationWithTrait Surface)
      )
    . (preyL .~ Prey (MostToken AlarmLevel <> HasTokens AlarmLevel (atLeast 3)))

instance RunMessage MoonboundByakhee where
  runMessage msg (MoonboundByakhee attrs) =
    MoonboundByakhee <$> runMessage msg attrs
