module Arkham.Enemy.Cards.OtherworldlyMeddler (
  otherworldlyMeddler,
  OtherworldlyMeddler (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype OtherworldlyMeddler = OtherworldlyMeddler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

otherworldlyMeddler :: EnemyCard OtherworldlyMeddler
otherworldlyMeddler = enemy OtherworldlyMeddler Cards.otherworldlyMeddler (4, PerPlayer 5, 3) (2, 2)

instance HasAbilities OtherworldlyMeddler where
  getAbilities (OtherworldlyMeddler attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ EnemyWithId (toId attrs) <> EnemyWithAnyDoom)
          $ ForcedAbility
          $ EnemyTakeDamage #when Matcher.AttackDamageEffect (EnemyWithId $ toId attrs) AnyValue AnySource
      , mkAbility attrs 2 $ ForcedAbility $ InvestigatorDefeated #after ByAny Anyone
      ]

instance RunMessage OtherworldlyMeddler where
  runMessage msg e@(OtherworldlyMeddler attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      replaceMessageMatching
        \case
          EnemyDamaged eid _ -> eid == toId attrs
          _ -> False
        \case
          EnemyDamaged eid dmg -> [EnemyDamaged eid (dmg {damageAssignmentAmount = max 0 (damageAssignmentAmount dmg - 1)})]
          _ -> error "invalid match"
      push $ RemoveDoom (toAbilitySource attrs 1) (toTarget attrs) 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ PlaceDoom (toAbilitySource attrs 2) (toTarget attrs) 3
      pure e
    _ -> OtherworldlyMeddler <$> runMessage msg attrs
