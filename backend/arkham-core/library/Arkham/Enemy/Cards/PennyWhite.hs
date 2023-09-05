module Arkham.Enemy.Cards.PennyWhite (
  pennyWhite,
  PennyWhite (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype PennyWhite = PennyWhite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennyWhite :: EnemyCard PennyWhite
pennyWhite = enemy PennyWhite Cards.pennyWhite (4, Static 5, 3) (0, 2)

instance HasAbilities PennyWhite where
  getAbilities (PennyWhite a) =
    withBaseAbilities a $
      [ mkAbility a 1 $
          ForcedAbility $
            Matcher.EnemyEvaded Timing.After You $
              EnemyWithId $
                toId a
      ]

instance RunMessage PennyWhite where
  runMessage msg e@(PennyWhite attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toAbilitySource attrs 1) DamageAny 1 0
      pure e
    _ -> PennyWhite <$> runMessage msg attrs
