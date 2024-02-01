module Arkham.Enemy.Cards.Narogath (
  narogath,
  Narogath (..),
) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Trait
import Arkham.Trait qualified as Trait

newtype Narogath = Narogath EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

narogath :: EnemyCard Narogath
narogath =
  enemyWith Narogath Cards.narogath (3, Static 4, 3) (1, 2)
    $ preyL
    .~ Prey
      ( NearestToEnemy
          $ EnemyWithTrait Trait.Cultist
          <> NotEnemy
            (enemyIs Cards.narogath)
      )

instance HasModifiersFor Narogath where
  getModifiersFor (InvestigatorTarget iid) (Narogath a) = do
    affected <-
      iid
        <=~> InvestigatorAt (AccessibleFrom $ locationWithEnemy $ toId a)
    pure
      $ toModifiers
        a
        [ CannotTakeAction $ EnemyAction Parley $ EnemyWithTrait Cultist
        | enemyReady a && affected
        ]
  getModifiersFor target (Narogath a) | isTarget a target = do
    n <- getPlayerCountValue $ PerPlayer 3
    pure $ toModifiers a [HealthModifier n]
  getModifiersFor _ _ = pure []

instance RunMessage Narogath where
  runMessage msg (Narogath attrs) = Narogath <$> runMessage msg attrs
