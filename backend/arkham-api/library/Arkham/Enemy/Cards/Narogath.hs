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
  getModifiersFor (InvestigatorTarget iid) (Narogath a) = do
    affected <- iid <=~> InvestigatorAt (AccessibleFrom $ locationWithEnemy a.id)
    toModifiers
      a
      [ CannotTakeAction $ EnemyAction Parley $ EnemyWithTrait Cultist
      | a.ready && affected
      ]
  getModifiersFor target (Narogath a) | isTarget a target = do
    n <- getPlayerCountValue $ PerPlayer 3
    toModifiers a [HealthModifier n]
  getModifiersFor _ _ = pure []

instance RunMessage Narogath where
  runMessage msg (Narogath attrs) = Narogath <$> runMessage msg attrs
