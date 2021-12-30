module Arkham.Enemy.Cards.AcolyteOfUmordhoth where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Prey
import Arkham.Query
import Arkham.Target

newtype AcolyteOfUmordhoth = AcolyteOfUmordhoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

acolyteOfUmordhoth :: EnemyCard AcolyteOfUmordhoth
acolyteOfUmordhoth = enemyWith
  AcolyteOfUmordhoth
  Cards.acolyteOfUmordhoth
  (3, Static 3, 2)
  (1, 1)
  (preyL .~ FewestCards)

instance HasCount CardCount env InvestigatorId => HasModifiersFor env AcolyteOfUmordhoth where
  getModifiersFor _ (EnemyTarget eid) (AcolyteOfUmordhoth a@EnemyAttrs {..})
    | eid == enemyId = do
      anyWithoutCards <- or <$> for
        (setToList enemyEngagedInvestigators)
        (\iid -> (== 0) . unCardCount <$> getCount iid)
      pure $ toModifiers a [ CannotBeEvaded | anyWithoutCards ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env AcolyteOfUmordhoth where
  runMessage msg (AcolyteOfUmordhoth attrs) =
    AcolyteOfUmordhoth <$> runMessage msg attrs
