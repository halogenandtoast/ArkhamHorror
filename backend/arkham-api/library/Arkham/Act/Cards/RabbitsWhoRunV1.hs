module Arkham.Act.Cards.RabbitsWhoRunV1 (rabbitsWhoRunV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Attack (enemyAttack)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Scenarios.DogsOfWar.Helpers

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
      let enemy :: EnemyId = toResult v
      -- "each ready enemy attacks each Key Locus at its location as if it were
      -- an engaged investigator." Model this as a real attack so attack-reaction
      -- cards (Dodge etc.) can interact with it; PerformEnemyAttack folds the
      -- enemy's horror into the asset damage.
      selectOne (locationWithEnemy enemy) >>= traverse_ \lid ->
        selectEach (assetAt lid <> AssetWithModifier IsKeyLocus) \locus ->
          push $ InitiateEnemyAttack $ enemyAttack enemy enemy locus
      pure a
    _ -> RabbitsWhoRunV1 <$> liftRunMessage msg attrs
