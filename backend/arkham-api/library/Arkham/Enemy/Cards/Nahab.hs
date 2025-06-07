module Arkham.Enemy.Cards.Nahab (nahab) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Agenda
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Nahab = Nahab EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nahab :: EnemyCard Nahab
nahab = enemy Nahab Cards.nahab (1, PerPlayer 1, 3) (1, 2)

-- Do not remove doom from Nahab when the agenda advances.
instance HasModifiersFor Nahab where
  getModifiersFor (Nahab attrs) = do
    agendaStep <- getCurrentAgendaStep
    modifySelf attrs [DoNotRemoveDoom, EnemyFight agendaStep]

instance HasAbilities Nahab where
  getAbilities (Nahab a) =
    extend1 a
      $ restricted a 1 (EnemyCriteria $ ThisEnemy ReadyEnemy)
      $ forced
      $ PhaseBegins #after #enemy

instance RunMessage Nahab where
  runMessage msg e@(Nahab attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom attrs attrs 1
      pure e
    _ -> Nahab <$> liftRunMessage msg attrs
