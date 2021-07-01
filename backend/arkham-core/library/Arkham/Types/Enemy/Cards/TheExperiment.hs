module Arkham.Types.Enemy.Cards.TheExperiment where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query

newtype TheExperiment = TheExperiment EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theExperiment :: EnemyCard TheExperiment
theExperiment = enemy TheExperiment Cards.theExperiment
  $ (healthDamageL .~ 2)
  . (sanityDamageL .~ 2)
  . (fightL .~ 4)
  . (healthL .~ Static 7)
  . (evadeL .~ 2)

instance HasCount PlayerCount env () => HasModifiersFor env TheExperiment where
  getModifiersFor _ target (TheExperiment attrs) | isTarget attrs target = do
    modifier <- getPlayerCountValue (PerPlayer 3)
    pure $ toModifiers attrs [HealthModifier modifier]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TheExperiment where
  getActions i window (TheExperiment attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env TheExperiment where
  runMessage msg (TheExperiment attrs) = case msg of
    EnemyDefeated eid _ _ _ _ _ | eid == enemyId attrs -> do
      actId <- fromJustNote "missing act" . headMay <$> getSetList ()
      unshiftMessage (AdvanceAct actId (toSource attrs))
      TheExperiment <$> runMessage msg attrs
    BeginEnemy -> TheExperiment <$> runMessage ReadyExhausted attrs
    _ -> TheExperiment <$> runMessage msg attrs
