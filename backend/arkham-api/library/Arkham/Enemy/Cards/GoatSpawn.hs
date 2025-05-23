module Arkham.Enemy.Cards.GoatSpawn (goatSpawn) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher

newtype GoatSpawn = GoatSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

goatSpawn :: EnemyCard GoatSpawn
goatSpawn = enemy GoatSpawn Cards.goatSpawn (3, Static 3, 2) (1, 0)

instance HasAbilities GoatSpawn where
  getAbilities (GoatSpawn a) =
    extend1 a $ forcedAbility a 1 $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage GoatSpawn where
  runMessage msg e@(GoatSpawn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt $ locationWithEnemy attrs) (assignHorrorTo (attrs.ability 1) 1)
      pure e
    _ -> GoatSpawn <$> liftRunMessage msg attrs
