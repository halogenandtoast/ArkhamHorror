module Arkham.Types.Enemy.Cards.YithianObserver
  ( YithianObserver(..)
  , yithianObserver
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks)
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Timing qualified as Timing

newtype YithianObserver = YithianObserver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianObserver :: EnemyCard YithianObserver
yithianObserver = enemyWith
  YithianObserver
  Cards.yithianObserver
  (4, Static 4, 3)
  (1, 1)
  (preyL .~ FewestCards)

instance HasAbilities YithianObserver where
  getAbilities (YithianObserver a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyAttacks Timing.When You
      $ EnemyWithId
      $ toId a
    ]

instance EnemyRunner env => RunMessage env YithianObserver where
  runMessage msg e@(YithianObserver attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      cardCount' <- unCardCount <$> getCount iid
      if cardCount' == 0
        then
          e
            <$ push
                 (skillTestModifiers
                   source
                   (toTarget attrs)
                   [DamageDealt 1, HorrorDealt 1]
                 )
        else e <$ push (RandomDiscard iid)
    _ -> YithianObserver <$> runMessage msg attrs
