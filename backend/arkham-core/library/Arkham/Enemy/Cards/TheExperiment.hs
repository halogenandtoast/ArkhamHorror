module Arkham.Enemy.Cards.TheExperiment
  ( TheExperiment(..)
  , theExperiment
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Phase
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

instance HasModifiersFor TheExperiment where
  getModifiersFor target (TheExperiment attrs) | isTarget attrs target = do
    modifier <- getPlayerCountValue (PerPlayer 3)
    pure $ toModifiers attrs [HealthModifier modifier]
  getModifiersFor _ _ = pure []

instance RunMessage TheExperiment where
  runMessage msg e@(TheExperiment attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      e <$ push (Ready $ toTarget attrs)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      actId <- selectJust AnyAct
      e <$ push (AdvanceAct actId (toSource attrs) AdvancedWithOther)
    _ -> TheExperiment <$> runMessage msg attrs
