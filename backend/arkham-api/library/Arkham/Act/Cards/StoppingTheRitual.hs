module Arkham.Act.Cards.StoppingTheRitual (StoppingTheRitual (..), stoppingTheRitual) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype StoppingTheRitual = StoppingTheRitual ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoppingTheRitual :: ActCard StoppingTheRitual
stoppingTheRitual =
  act (3, A) StoppingTheRitual Cards.stoppingTheRitual Nothing

instance HasModifiersFor StoppingTheRitual where
  getModifiersFor (StoppingTheRitual a) =
    modifySelect a (enemyIs Enemies.nahab) [CannotMove]

instance HasAbilities StoppingTheRitual where
  getAbilities (StoppingTheRitual a)
    | onSide A a =
        [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny $ enemyIs Enemies.nahab
        , restricted a 2 (exists $ enemyIs Enemies.nahab <> not_ EnemyWithAnyDoom)
            $ Objective
            $ forced AnyWindow
        ]
  getAbilities _ = []

instance RunMessage StoppingTheRitual where
  runMessage msg a@(StoppingTheRitual attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      nahab <- selectJust $ enemyIs Enemies.nahab
      cancelEnemyDefeat nahab
      healAllDamage attrs nahab
      disengageFromAll nahab
      exhaustThis nahab
      roundModifier attrs nahab DoesNotReadyDuringUpkeep
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> StoppingTheRitual <$> liftRunMessage msg attrs
