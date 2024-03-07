module Arkham.Agenda.Cards.TheDeadWithNoName (TheDeadWithNoName (..), theDeadWithNoName) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Keyword (Keyword (Aloof, Massive, Swarming))
import Arkham.Matcher
import Arkham.Prelude

newtype TheDeadWithNoName = TheDeadWithNoName AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDeadWithNoName :: AgendaCard TheDeadWithNoName
theDeadWithNoName = agenda (3, A) TheDeadWithNoName Cards.theDeadWithNoName (Static 7)

instance HasModifiersFor TheDeadWithNoName where
  getModifiersFor (EnemyTarget eid) (TheDeadWithNoName a) = do
    isRat <- eid <=~> enemyIs Enemies.swarmOfRats
    isUnnamable <- eid <=~> enemyIs Enemies.theUnnamable
    pure
      $ toModifiers a
      $ (guard isRat $> AddKeyword (Swarming (Static 2)))
      <> (guard isUnnamable *> [RemoveKeyword Aloof, AddKeyword Massive])
  getModifiersFor _ _ = pure []

instance HasAbilities TheDeadWithNoName where
  getAbilities (TheDeadWithNoName a) =
    [ restrictedAbility a 1 (exists $ enemyIs Enemies.theUnnamable <> EnemyAt YourLocation <> ReadyEnemy)
        $ forced
        $ TurnBegins #when You
    ]

instance RunMessage TheDeadWithNoName where
  runMessage msg a@(TheDeadWithNoName attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push R2
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (attrs.ability 1) iid #willpower 2
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      theUnnamable <- selectJust $ enemyIs Enemies.theUnnamable
      push $ InitiateEnemyAttack $ enemyAttack theUnnamable (attrs.ability 1) iid
      pure a
    _ -> TheDeadWithNoName <$> runMessage msg attrs
