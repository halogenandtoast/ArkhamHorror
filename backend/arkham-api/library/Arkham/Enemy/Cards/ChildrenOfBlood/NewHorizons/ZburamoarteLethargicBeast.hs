module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.ZburamoarteLethargicBeast (zburamoarteLethargicBeast) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher

newtype ZburamoarteLethargicBeast = ZburamoarteLethargicBeast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zburamoarteLethargicBeast :: EnemyCard ZburamoarteLethargicBeast
zburamoarteLethargicBeast = enemy ZburamoarteLethargicBeast Cards.zburamoarteLethargicBeast

instance HasModifiersFor ZburamoarteLethargicBeast where
  getModifiersFor (ZburamoarteLethargicBeast a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (3 * n), CannotMove]

instance HasAbilities ZburamoarteLethargicBeast where
  getAbilities (ZburamoarteLethargicBeast a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDealsDamage #after (be a)

instance RunMessage ZburamoarteLethargicBeast where
  runMessage msg e@(ZburamoarteLethargicBeast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> ZburamoarteLethargicBeast <$> liftRunMessage msg attrs
