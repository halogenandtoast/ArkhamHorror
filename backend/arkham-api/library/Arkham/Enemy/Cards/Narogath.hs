module Arkham.Enemy.Cards.Narogath (narogath, Narogath (..)) where

import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait
import Arkham.Trait qualified as Trait

newtype Narogath = Narogath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

narogath :: EnemyCard Narogath
narogath =
  enemyWith Narogath Cards.narogath (3, Static 4, 3) (1, 2)
    $ preyL
    .~ Prey (NearestToEnemy $ withTrait Trait.Cultist <> not_ (enemyIs Cards.narogath))

instance HasModifiersFor Narogath where
  getModifiersFor (Narogath a) = do
    investigators <-
      modifySelectWhen
        a
        a.ready
        (InvestigatorAt $ AccessibleFrom $ locationWithEnemy a)
        [CannotTakeAction $ EnemyAction Parley $ EnemyWithTrait Cultist]
    n <- perPlayer 3
    self <- modifySelf a [HealthModifier n]
    pure $ investigators <> self

instance RunMessage Narogath where
  runMessage msg (Narogath attrs) = Narogath <$> runMessage msg attrs
