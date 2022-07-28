module Arkham.Enemy.Cards.YithianObserver
  ( YithianObserver(..)
  , yithianObserver
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message hiding ( EnemyAttacks )
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype YithianObserver = YithianObserver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianObserver :: EnemyCard YithianObserver
yithianObserver = enemyWith
  YithianObserver
  Cards.yithianObserver
  (4, Static 4, 3)
  (1, 1)
  (preyL .~ Prey FewestCardsInHand)

instance HasAbilities YithianObserver where
  getAbilities (YithianObserver a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyAttacks Timing.When You AnyEnemyAttack
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage YithianObserver where
  runMessage msg e@(YithianObserver attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      emptyHand<- fieldMap InvestigatorHand null iid
      if emptyHand
        then push $ skillTestModifiers
          source
          (toTarget attrs)
          [DamageDealt 1, HorrorDealt 1]
        else push (RandomDiscard iid)
      pure e
    _ -> YithianObserver <$> runMessage msg attrs
