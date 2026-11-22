module Arkham.Act.Cards.TheScarletKeys.DogsOfWar.RabbitsWhoRunV1 (rabbitsWhoRunV1) where

import Arkham.Act.CardDefs.TheScarletKeys.DogsOfWar qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Attack (enemyAttack)
import Arkham.GameEnv (getEnemyPhaseStep)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Phase (EnemyPhaseStep (ResolveAttacksStep))
import Arkham.Scenarios.TheScarletKeys.DogsOfWar.Helpers

newtype RabbitsWhoRunV1 = RabbitsWhoRunV1 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

rabbitsWhoRunV1 :: ActCard RabbitsWhoRunV1
rabbitsWhoRunV1 = act (1, A) RabbitsWhoRunV1 Cards.rabbitsWhoRunV1 Nothing

instance HasModifiersFor RabbitsWhoRunV1 where
  getModifiersFor (RabbitsWhoRunV1 a) = do
    modifySelect a locationWithKeyLocus [CountsAsInvestigatorForHunterEnemies]

instance RunMessage RabbitsWhoRunV1 where
  runMessage msg a@(RabbitsWhoRunV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (enemy_ "The Beast in a Cowl of Crimson") addToVictory_
      selectEach (LocationWithModifier IsKeyLocus) addToVictory_
      selectEach (AssetWithModifier IsKeyLocus) addToVictory_
      push R1
      pure a
    ScenarioSpecific "enemyAttacked" v -> do
      -- Only the "enemies attack" step, #6030. ScenarioSpecific "enemyAttacked"
      -- is emitted for every attack, retaliate and alert included.
      step <- getEnemyPhaseStep
      when (step == Just ResolveAttacksStep) do
        let enemy :: EnemyId = toResult v
        -- A real attack, so Dodge etc. can cancel it; PerformEnemyAttack folds
        -- the enemy's horror into the asset damage.
        selectOne (locationWithEnemy enemy) >>= traverse_ \lid ->
          selectEach (assetAt lid <> AssetWithModifier IsKeyLocus) \locus ->
            push $ InitiateEnemyAttack $ enemyAttack enemy enemy locus
      pure a
    _ -> RabbitsWhoRunV1 <$> liftRunMessage msg attrs
