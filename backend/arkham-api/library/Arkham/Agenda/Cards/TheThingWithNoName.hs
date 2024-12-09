module Arkham.Agenda.Cards.TheThingWithNoName (TheThingWithNoName (..), theThingWithNoName) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Keyword (Keyword (Swarming))
import Arkham.Matcher
import Arkham.Prelude

newtype TheThingWithNoName = TheThingWithNoName AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingWithNoName :: AgendaCard TheThingWithNoName
theThingWithNoName = agenda (2, A) TheThingWithNoName Cards.theThingWithNoName (Static 6)

instance HasModifiersFor TheThingWithNoName where
  getModifiersFor (TheThingWithNoName a) = do
    modifySelect a (enemyIs Enemies.swarmOfRats) [AddKeyword (Swarming (Static 1))]

instance HasAbilities TheThingWithNoName where
  getAbilities (TheThingWithNoName a) =
    [ skillTestAbility
        $ restrictedAbility a 1 (exists $ enemyIs Enemies.theUnnamable <> EnemyAt YourLocation <> ReadyEnemy)
        $ forced
        $ TurnBegins #when You
    ]

instance RunMessage TheThingWithNoName where
  runMessage msg a@(TheThingWithNoName attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      pushAll [ShuffleEncounterDiscardBackIn, advanceAgendaDeck attrs]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      theUnnamable <- selectJust $ enemyIs Enemies.theUnnamable
      push $ InitiateEnemyAttack $ enemyAttack theUnnamable (attrs.ability 1) iid
      pure a
    _ -> TheThingWithNoName <$> runMessage msg attrs
