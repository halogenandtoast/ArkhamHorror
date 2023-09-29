module Arkham.Enemy.Cards.AcolyteOfUmordhoth where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype AcolyteOfUmordhoth = AcolyteOfUmordhoth EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

acolyteOfUmordhoth :: EnemyCard AcolyteOfUmordhoth
acolyteOfUmordhoth =
  enemyWith AcolyteOfUmordhoth Cards.acolyteOfUmordhoth (3, Static 3, 2) (1, 1)
    $ preyL
    .~ Prey FewestCardsInHand

instance HasModifiersFor AcolyteOfUmordhoth where
  getModifiersFor target (AcolyteOfUmordhoth a) | a `is` target = do
    enemyEngagedInvestigators <- selectList $ investigatorEngagedWith (toId a)
    anyWithoutCards <- or <$> traverse (fieldMap InvestigatorHand null) enemyEngagedInvestigators
    pure $ toModifiers a [CannotBeEvaded | anyWithoutCards]
  getModifiersFor _ _ = pure []

instance RunMessage AcolyteOfUmordhoth where
  runMessage msg (AcolyteOfUmordhoth attrs) = AcolyteOfUmordhoth <$> runMessage msg attrs
