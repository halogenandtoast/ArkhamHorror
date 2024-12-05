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
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDeadWithNoName :: AgendaCard TheDeadWithNoName
theDeadWithNoName = agenda (3, A) TheDeadWithNoName Cards.theDeadWithNoName (Static 7)

instance HasModifiersFor TheDeadWithNoName where
  getModifiersFor (TheDeadWithNoName a) = do
    rats <- modifySelect a (enemyIs Enemies.swarmOfRats) [AddKeyword (Swarming (Static 2))]
    unnamable <- modifySelect a (enemyIs Enemies.theUnnamable) [RemoveKeyword Aloof, AddKeyword Massive]
    pure $ rats <> unnamable

instance HasAbilities TheDeadWithNoName where
  getAbilities (TheDeadWithNoName a) =
    [ skillTestAbility
        $ restricted a 1 (exists $ enemyIs Enemies.theUnnamable <> at_ YourLocation <> ReadyEnemy)
        $ forced
        $ TurnBegins #when You
    ]

instance RunMessage TheDeadWithNoName where
  runMessage msg a@(TheDeadWithNoName attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push R2
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      theUnnamable <- selectJust $ enemyIs Enemies.theUnnamable
      push $ InitiateEnemyAttack $ enemyAttack theUnnamable (attrs.ability 1) iid
      pure a
    _ -> TheDeadWithNoName <$> runMessage msg attrs
