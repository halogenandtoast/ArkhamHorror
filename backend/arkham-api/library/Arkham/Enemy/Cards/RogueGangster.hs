module Arkham.Enemy.Cards.RogueGangster (rogueGangster) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype RogueGangster = RogueGangster EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rogueGangster :: EnemyCard RogueGangster
rogueGangster =
  enemy RogueGangster Cards.rogueGangster (3, Static 3, 3) (1, 0)
    & setPrey MostResources
instance HasAbilities RogueGangster where
  getAbilities (RogueGangster a) = extend1 a $ forcedAbility a 1 $ EnemyEngaged #after You (be a)

instance RunMessage RogueGangster where
  runMessage msg e@(RogueGangster attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
        resources <- field InvestigatorResources iid
        if (resources >= 1)
            then loseResources iid (attrs.ability 1) 1
            else initiateEnemyAttack attrs (attrs.ability 1) iid
        pure e
    _ -> RogueGangster <$> liftRunMessage msg attrs