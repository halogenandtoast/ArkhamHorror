module Arkham.Enemy.Cards.VengefulSpecter (
  vengefulSpecter,
  VengefulSpecter (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Trait (Trait (Charm, Relic, Spell))

newtype VengefulSpecter = VengefulSpecter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

vengefulSpecter :: EnemyCard VengefulSpecter
vengefulSpecter = enemy VengefulSpecter Cards.vengefulSpecter (4, PerPlayer 4, 5) (0, 2)

instance HasAbilities VengefulSpecter where
  getAbilities (VengefulSpecter attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ EnemyWithId (toId attrs) <> EnemyWithAnyDoom)
          $ ForcedAbility
          $ EnemyTakeDamage #when Matcher.AttackDamageEffect (EnemyWithId $ toId attrs) (atLeast 2)
          $ NotSource
          $ SourceMatchesAny [SourceWithTrait Spell, SourceWithTrait Relic, SourceWithTrait Charm]
      ]

instance RunMessage VengefulSpecter where
  runMessage msg e@(VengefulSpecter attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      replaceMessageMatching
        \case
          EnemyDamaged eid _ -> eid == toId attrs
          _ -> False
        \case
          EnemyDamaged eid dmg -> [EnemyDamaged eid (dmg {damageAssignmentAmount = 1})]
          _ -> error "invalid match"
      pure e
    _ -> VengefulSpecter <$> runMessage msg attrs
