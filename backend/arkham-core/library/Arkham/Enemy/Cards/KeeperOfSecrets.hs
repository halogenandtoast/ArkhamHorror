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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
      push $ parley iid (attrs.ability 2) attrs #intellect (Fixed 3)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      push $ RemoveAllDoom (toAbilitySource attrs 2) (toTarget attrs)
      pure e
    _ -> KeeperOfSecrets <$> runMessage msg attrs
