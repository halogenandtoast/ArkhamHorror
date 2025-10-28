module Arkham.Enemy.Cards.KeeperOfSecrets (keeperOfSecrets) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher

newtype KeeperOfSecrets = KeeperOfSecrets EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keeperOfSecrets :: EnemyCard KeeperOfSecrets
keeperOfSecrets =
  enemy KeeperOfSecrets Cards.keeperOfSecrets (4, Static 2, 3) (1, 1)
    & setSpawnAt EmptyLocation

instance HasAbilities KeeperOfSecrets where
  getAbilities (KeeperOfSecrets a) =
    extend
      a
      [ restricted a 1 CanPlaceDoomOnThis $ forced $ PhaseEnds #when #mythos
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage KeeperOfSecrets where
  runMessage msg e@(KeeperOfSecrets attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 3)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      removeAllDoom (attrs.ability 2) attrs
      pure e
    _ -> KeeperOfSecrets <$> liftRunMessage msg attrs
