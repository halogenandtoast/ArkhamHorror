module Arkham.Enemy.Cards.PersistentConstruct (persistentConstruct) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Matcher
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.Trait (Trait (Artifact))

newtype PersistentConstruct = PersistentConstruct EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

persistentConstruct :: EnemyCard PersistentConstruct
persistentConstruct =
  enemyWith PersistentConstruct Cards.persistentConstruct
    $ preyL
    .~ Prey (HasMatchingAsset (AssetWithTrait Artifact))

instance HasAbilities PersistentConstruct where
  getAbilities (PersistentConstruct a) =
    extend1 a
      $ restricted a 1 (InVictoryDisplay (cardIs Cards.persistentConstruct) (atLeast 1))
      $ forced
      $ EnemyDefeated #when You ByAny (be a)

instance RunMessage PersistentConstruct where
  runMessage msg e@(PersistentConstruct attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      others <-
        scenarioFieldMap ScenarioVictoryDisplay
          $ filter (`cardMatch` cardIs Cards.persistentConstruct)
      withLocationOf iid \lid ->
        for_ others \card -> createEnemyAtEdit_ card lid createExhausted
      pure e
    _ -> PersistentConstruct <$> liftRunMessage msg attrs
