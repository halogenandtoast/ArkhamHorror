module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.ZburamoarteTheSourceOfTheBlight (zburamoarteTheSourceOfTheBlight) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher

newtype ZburamoarteTheSourceOfTheBlight = ZburamoarteTheSourceOfTheBlight EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zburamoarteTheSourceOfTheBlight :: EnemyCard ZburamoarteTheSourceOfTheBlight
zburamoarteTheSourceOfTheBlight = enemy ZburamoarteTheSourceOfTheBlight Cards.zburamoarteTheSourceOfTheBlight

instance HasModifiersFor ZburamoarteTheSourceOfTheBlight where
  getModifiersFor (ZburamoarteTheSourceOfTheBlight a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (3 * n), CannotMove]

instance HasAbilities ZburamoarteTheSourceOfTheBlight where
  getAbilities (ZburamoarteTheSourceOfTheBlight a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDealsDamage #after (be a)

instance RunMessage ZburamoarteTheSourceOfTheBlight where
  runMessage msg e@(ZburamoarteTheSourceOfTheBlight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getPlayerCount
      healDamage attrs (attrs.ability 1) n
      pure e
    _ -> ZburamoarteTheSourceOfTheBlight <$> liftRunMessage msg attrs
