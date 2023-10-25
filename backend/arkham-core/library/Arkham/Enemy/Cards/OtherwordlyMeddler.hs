module Arkham.Enemy.Cards.OtherwordlyMeddler (
  otherwordlyMeddler,
  OtherwordlyMeddler (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype OtherwordlyMeddler = OtherwordlyMeddler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherwordlyMeddler :: EnemyCard OtherwordlyMeddler
otherwordlyMeddler = enemy OtherwordlyMeddler Cards.otherwordlyMeddler (4, PerPlayer 5, 3) (2, 2)

instance HasAbilities OtherwordlyMeddler where
  getAbilities (OtherwordlyMeddler attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ EnemyWithId (toId attrs) <> EnemyWithAnyDoom)
          $ ForcedAbility
          $ EnemyTakeDamage #when Matcher.AttackDamageEffect (EnemyWithId $ toId attrs) AnySource
      , mkAbility attrs 2 $ ForcedAbility $ InvestigatorDefeated #after ByAny Anyone
      ]

instance RunMessage OtherwordlyMeddler where
  runMessage msg e@(OtherwordlyMeddler attrs) = case msg of
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
    _ -> OtherwordlyMeddler <$> runMessage msg attrs
