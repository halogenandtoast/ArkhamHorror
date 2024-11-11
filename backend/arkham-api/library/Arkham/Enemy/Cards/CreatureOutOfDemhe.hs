module Arkham.Enemy.Cards.CreatureOutOfDemhe (creatureOutOfDemhe, CreatureOutOfDemhe (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Window
import Arkham.Window qualified as Window

newtype CreatureOutOfDemhe = CreatureOutOfDemhe EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creatureOutOfDemhe :: EnemyCard CreatureOutOfDemhe
creatureOutOfDemhe =
  enemyWith CreatureOutOfDemhe Cards.creatureOutOfDemhe (5, Static 4, 2) (1, 1)
    $ spawnAtL
    ?~ SpawnAt "Depths of Demhe"

instance HasAbilities CreatureOutOfDemhe where
  getAbilities (CreatureOutOfDemhe a) =
    extend1 a $ mkAbility a 1 $ forced $ Matcher.FlipLocation #when Anyone $ orConnected LocationOfThis

instance RunMessage CreatureOutOfDemhe where
  runMessage msg e@(CreatureOutOfDemhe attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.FlipLocation _ lid)] _ -> do
      selectEach (InvestigatorAt $ LocationWithId lid) $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    _ -> CreatureOutOfDemhe <$> liftRunMessage msg attrs
