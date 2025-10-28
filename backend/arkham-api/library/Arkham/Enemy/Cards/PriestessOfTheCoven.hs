module Arkham.Enemy.Cards.PriestessOfTheCoven (priestessOfTheCoven) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Trait (Trait (Witch))

newtype PriestessOfTheCoven = PriestessOfTheCoven EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

priestessOfTheCoven :: EnemyCard PriestessOfTheCoven
priestessOfTheCoven = enemy PriestessOfTheCoven Cards.priestessOfTheCoven (2, Static 3, 2) (2, 0)

instance HasModifiersFor PriestessOfTheCoven where
  getModifiersFor (PriestessOfTheCoven a) = do
    witchCount <- length <$> findInDiscard (CardWithTrait Witch)
    modifySelfWhen
      a
      (witchCount > 0)
      [ Modifier.EnemyFight (min 3 witchCount)
      , Modifier.EnemyEvade (min 3 witchCount)
      ]

instance HasAbilities PriestessOfTheCoven where
  getAbilities (PriestessOfTheCoven a) = extend1 a $ mkAbility a 1 $ forced EncounterDeckRunsOutOfCards

instance RunMessage PriestessOfTheCoven where
  runMessage msg e@(PriestessOfTheCoven attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      iids <- select $ InvestigatorAt $ locationWithEnemy attrs
      for_ iids $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    _ -> PriestessOfTheCoven <$> liftRunMessage msg attrs
