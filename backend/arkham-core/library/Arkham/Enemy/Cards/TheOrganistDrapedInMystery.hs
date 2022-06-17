module Arkham.Enemy.Cards.TheOrganistDrapedInMystery
  ( theOrganistDrapedInMystery
  , TheOrganistDrapedInMystery(..)
  ) where

import Arkham.Prelude

import Arkham.Scenarios.APhantomOfTruth.Helpers
import Arkham.Ability
import Arkham.Classes
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Enemy.Helpers
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import qualified Arkham.Timing as Timing

newtype TheOrganistDrapedInMystery = TheOrganistDrapedInMystery EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheOrganistDrapedInMystery where
  getModifiersFor _ target (TheOrganistDrapedInMystery attrs)
    | isTarget attrs target = pure $ toModifiers attrs [CannotBeDamaged]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TheOrganistDrapedInMystery where
  getAbilities (TheOrganistDrapedInMystery attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ PhaseEnds Timing.After $ PhaseIs
        EnemyPhase
    ]

theOrganistDrapedInMystery :: EnemyCard TheOrganistDrapedInMystery
theOrganistDrapedInMystery = enemy
  TheOrganistDrapedInMystery
  Cards.theOrganistDrapedInMystery
  (3, Static 1, 5)
  (0, 1)

instance RunMessage TheOrganistDrapedInMystery where
  runMessage msg e@(TheOrganistDrapedInMystery attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      if null (enemyEngagedInvestigators attrs)
        then push =<< moveOrganistAwayFromNearestInvestigator
        else pushAll [ DisengageEnemy iid $ toId attrs | iid <- setToList (enemyEngagedInvestigators attrs) ]
      pure e
    _ -> TheOrganistDrapedInMystery <$> runMessage msg attrs
