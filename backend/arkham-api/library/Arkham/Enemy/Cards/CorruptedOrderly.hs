module Arkham.Enemy.Cards.CorruptedOrderly (corruptedOrderly, corruptedOrderlyEffect, CorruptedOrderly (..)) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype CorruptedOrderly = CorruptedOrderly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corruptedOrderly :: EnemyCard CorruptedOrderly
corruptedOrderly = enemy CorruptedOrderly Cards.corruptedOrderly (2, Static 2, 2) (1, 1)

instance HasAbilities CorruptedOrderly where
  getAbilities (CorruptedOrderly x) =
    extend
      x
      [ mkAbility x 1
          $ forced
          $ EnemyTakeDamage #when AnyDamageEffect (be x <> EnemyAt InfestedLocation) AnyValue AnySource
      ]

instance RunMessage CorruptedOrderly where
  runMessage msg e@(CorruptedOrderly attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push =<< createCardEffect Cards.corruptedOrderly Nothing (attrs.ability 1) attrs
      pure e
    _ -> CorruptedOrderly <$> runMessage msg attrs

newtype CorruptedOrderlyEffect = CorruptedOrderlyEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corruptedOrderlyEffect :: EffectArgs -> CorruptedOrderlyEffect
corruptedOrderlyEffect = cardEffect CorruptedOrderlyEffect Cards.corruptedOrderly

instance HasModifiersFor CorruptedOrderlyEffect where
  getModifiersFor (CorruptedOrderlyEffect attrs) =
    modified_ attrs attrs.target [DamageTaken (-1)]

isTakeDamage :: EffectAttrs -> Window -> Bool
isTakeDamage attrs window = case attrs.target of
  EnemyTarget eid -> go eid
  _ -> False
 where
  go eid = case windowType window of
    Window.TakeDamage _ _ (EnemyTarget eid') _ ->
      eid == eid' && window.timing == #after
    _ -> False

instance RunMessage CorruptedOrderlyEffect where
  runMessage msg e@(CorruptedOrderlyEffect attrs) = case msg of
    Do (CheckWindows windows') | any (isTakeDamage attrs) windows' -> do
      push $ disable attrs
      pure e
    _ -> CorruptedOrderlyEffect <$> runMessage msg attrs
