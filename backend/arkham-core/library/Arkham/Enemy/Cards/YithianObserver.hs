module Arkham.Enemy.Cards.YithianObserver (
  YithianObserver (..),
  yithianObserver,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype YithianObserver = YithianObserver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

yithianObserver :: EnemyCard YithianObserver
yithianObserver =
  enemyWith
    YithianObserver
    Cards.yithianObserver
    (4, Static 4, 3)
    (1, 1)
    (preyL .~ Prey FewestCardsInHand)

instance HasAbilities YithianObserver where
  getAbilities (YithianObserver a) =
    withBaseAbilities a
      $ [ forcedAbility a 1
            $ EnemyAttacks Timing.When You AnyEnemyAttack
            $ EnemyWithId (toId a)
        ]

instance RunMessage YithianObserver where
  runMessage msg e@(YithianObserver attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      emptyHand <- fieldMap InvestigatorHand null iid
      push
        $ if emptyHand
          then skillTestModifiers attrs attrs [DamageDealt 1, HorrorDealt 1]
          else toMessage $ randomDiscard iid attrs
      pure e
    _ -> YithianObserver <$> runMessage msg attrs
