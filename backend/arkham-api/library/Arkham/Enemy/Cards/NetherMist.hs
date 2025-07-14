module Arkham.Enemy.Cards.NetherMist (netherMist) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype NetherMist = NetherMist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities NetherMist where
  getAbilities (NetherMist a) =
    extend1 a $ haunted "Nether Mist attacks you." (proxied (LocationWithEnemy $ be a) a) 1

netherMist :: EnemyCard NetherMist
netherMist =
  enemy NetherMist Cards.netherMist (3, Static 4, 3) (1, 1)
    & setPrey (at_ $ LocationWithMostClues $ LocationWithInvestigator Anyone)

instance RunMessage NetherMist where
  runMessage msg e@(NetherMist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      initiateEnemyAttack (toId attrs) attrs iid
      pure e
    _ -> NetherMist <$> liftRunMessage msg attrs
