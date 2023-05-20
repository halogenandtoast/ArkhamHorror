module Arkham.Enemy.Cards.KeeperOfSecrets (
  keeperOfSecrets,
  KeeperOfSecrets (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.SkillTest.Base
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype KeeperOfSecrets = KeeperOfSecrets EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keeperOfSecrets :: EnemyCard KeeperOfSecrets
keeperOfSecrets =
  enemyWith
    KeeperOfSecrets
    Cards.keeperOfSecrets
    (4, Static 2, 3)
    (1, 1)
    (spawnAtL ?~ SpawnLocation EmptyLocation)

instance HasAbilities KeeperOfSecrets where
  getAbilities (KeeperOfSecrets a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 (Negate $ SelfHasModifier CannotPlaceDoomOnThis) $
          ForcedAbility $
            PhaseEnds Timing.When $
              PhaseIs MythosPhase
      , restrictedAbility a 2 OnSameLocation $
          ActionAbility (Just Action.Parley) $
            ActionCost 1
      ]

instance RunMessage KeeperOfSecrets where
  runMessage msg e@(KeeperOfSecrets attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
      pure e
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ BeginSkillTest $ initSkillTest iid attrs attrs SkillIntellect 3
      pure e
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ RemoveAllDoom (toAbilitySource attrs 2) (toTarget attrs)
      pure e
    _ -> KeeperOfSecrets <$> runMessage msg attrs
