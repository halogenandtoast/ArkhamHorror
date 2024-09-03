module Arkham.Enemy.Cards.GavriellaMizrah (
  gavriellaMizrah,
  GavriellaMizrah (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype GavriellaMizrah = GavriellaMizrah EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellaMizrah :: EnemyCard GavriellaMizrah
gavriellaMizrah = enemy GavriellaMizrah Cards.gavriellaMizrah (5, Static 4, 2) (2, 0)

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    withBaseAbilities a
      $ [ mkAbility a 1
            $ ForcedAbility
            $ EnemyAttackedSuccessfully Timing.After You AnySource
            $ EnemyWithId
            $ toId a
        ]

instance RunMessage GavriellaMizrah where
  runMessage msg e@(GavriellaMizrah attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toAbilitySource attrs 1) DamageAny 0 1
      pure e
    _ -> GavriellaMizrah <$> runMessage msg attrs
