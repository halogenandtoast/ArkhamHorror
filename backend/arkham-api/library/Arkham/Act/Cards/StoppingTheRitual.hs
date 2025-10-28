module Arkham.Act.Cards.StoppingTheRitual (stoppingTheRitual) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (insteadOfDiscarding)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (getEnemy)
import Arkham.Matcher

newtype StoppingTheRitual = StoppingTheRitual ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoppingTheRitual :: ActCard StoppingTheRitual
stoppingTheRitual = act (3, A) StoppingTheRitual Cards.stoppingTheRitual Nothing

instance HasModifiersFor StoppingTheRitual where
  getModifiersFor (StoppingTheRitual a) = modifySelect a (enemyIs Enemies.nahab) [CannotMove]

instance HasAbilities StoppingTheRitual where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny $ enemyIs Enemies.nahab
    , restricted a 2 (exists $ enemyIs Enemies.nahab <> not_ EnemyWithAnyDoom)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage StoppingTheRitual where
  runMessage msg a@(StoppingTheRitual attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getEnemy -> nahab) _ -> do
      insteadOfDiscarding nahab do
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
