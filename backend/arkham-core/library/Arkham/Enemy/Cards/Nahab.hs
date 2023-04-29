module Arkham.Enemy.Cards.Nahab (
  nahab,
  Nahab (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyFight)
import Arkham.Helpers.Agenda
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing

-- Do not remove doom from Nahab when the agenda advances.

newtype Nahab = Nahab EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nahab :: EnemyCard Nahab
nahab = enemy Nahab Cards.nahab (1, PerPlayer 1, 3) (1, 2)

instance HasModifiersFor Nahab where
  getModifiersFor target (Nahab attrs) | isTarget attrs target = do
    agendaStep <- getCurrentAgendaStep
    pure $ toModifiers attrs [DoNotRemoveDoom, EnemyFight agendaStep]
  getModifiersFor _ _ = pure []

instance HasAbilities Nahab where
  getAbilities (Nahab attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 (EnemyCriteria $ ThisEnemy ReadyEnemy) $
          ForcedAbility $
            PhaseBegins Timing.After $
              PhaseIs EnemyPhase
      ]

instance RunMessage Nahab where
  runMessage msg e@(Nahab attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceDoom (toTarget attrs) 1
      pure e
    _ -> Nahab <$> runMessage msg attrs
