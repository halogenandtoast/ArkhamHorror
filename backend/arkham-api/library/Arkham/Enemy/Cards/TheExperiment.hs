module Arkham.Enemy.Cards.TheExperiment (theExperiment) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype TheExperiment = TheExperiment EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theExperiment :: EnemyCard TheExperiment
theExperiment = enemy TheExperiment Cards.theExperiment (4, Static 7, 2) (2, 2)

instance HasAbilities TheExperiment where
  getAbilities (TheExperiment x) =
    extend
      x
      [ restricted x 1 (thisIs x $ enemy_ #exhausted) $ forced $ PhaseBegins #when #enemy
      , mkAbility x 2 $ Objective $ forced $ EnemyDefeated #when Anyone ByAny (be x)
      ]

instance HasModifiersFor TheExperiment where
  getModifiersFor (TheExperiment attrs) = do
    modifier <- getPlayerCountValue (PerPlayer 3)
    modifySelf attrs [HealthModifier modifier]

instance RunMessage TheExperiment where
  runMessage msg e@(TheExperiment attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      addToVictory attrs
      push $ AdvanceToAct 1 Acts.campusSafety B (toSource attrs)
      pure e
    _ -> TheExperiment <$> liftRunMessage msg attrs
