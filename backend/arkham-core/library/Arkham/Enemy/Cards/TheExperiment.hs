module Arkham.Enemy.Cards.TheExperiment
  ( TheExperiment(..)
  , theExperiment
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Modifier
import Arkham.Phase
import Arkham.Query
import Arkham.Timing qualified as Timing

newtype TheExperiment = TheExperiment EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theExperiment :: EnemyCard TheExperiment
theExperiment = enemy TheExperiment Cards.theExperiment (4, Static 7, 2) (2, 2)

instance HasAbilities TheExperiment where
  getAbilities (TheExperiment x) = withBaseAbilities
    x
    [ mkAbility x 1 $ ForcedAbility $ PhaseBegins Timing.When $ PhaseIs
      EnemyPhase
    , mkAbility x 2
    $ Objective
    $ ForcedAbility
    $ EnemyDefeated Timing.When You
    $ EnemyWithId
    $ toId x
    ]

instance HasCount PlayerCount env () => HasModifiersFor env TheExperiment where
  getModifiersFor _ target (TheExperiment attrs) | isTarget attrs target = do
    modifier <- getPlayerCountValue (PerPlayer 3)
    pure $ toModifiers attrs [HealthModifier modifier]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env TheExperiment where
  runMessage msg e@(TheExperiment attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Ready $ toTarget attrs)
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      actId <- fromJustNote "missing act" . headMay <$> getSetList ()
      e <$ push (AdvanceAct actId $ toSource attrs)
    _ -> TheExperiment <$> runMessage msg attrs
