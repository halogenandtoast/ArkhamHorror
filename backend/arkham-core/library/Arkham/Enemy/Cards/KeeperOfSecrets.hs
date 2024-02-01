module Arkham.Enemy.Cards.KeeperOfSecrets (
  keeperOfSecrets,
  KeeperOfSecrets (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype KeeperOfSecrets = KeeperOfSecrets EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

keeperOfSecrets :: EnemyCard KeeperOfSecrets
keeperOfSecrets =
  enemyWith KeeperOfSecrets Cards.keeperOfSecrets (4, Static 2, 3) (1, 1)
    $ spawnAtL
    ?~ SpawnAt EmptyLocation

instance HasAbilities KeeperOfSecrets where
  getAbilities (KeeperOfSecrets a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 CanPlaceDoomOnThis $ ForcedAbility $ PhaseEnds #when #mythos
        , restrictedAbility a 2 OnSameLocation parleyAction_
        ]

instance RunMessage KeeperOfSecrets where
  runMessage msg e@(KeeperOfSecrets attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ placeDoom (toAbilitySource attrs 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ beginSkillTest iid attrs attrs #intellect 3
      pure e
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      push $ RemoveAllDoom (toAbilitySource attrs 2) (toTarget attrs)
      pure e
    _ -> KeeperOfSecrets <$> runMessage msg attrs
