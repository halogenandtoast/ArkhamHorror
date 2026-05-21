module Arkham.Behavior.Damage where

import Arkham.DamageEffect (DamageEffect)
import Arkham.Message.Lifted (checkAfter, checkWhen)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Source (Source)
import Arkham.Strategy (DamageStrategy (DamageDirect))
import Arkham.Target (Target)
import Arkham.Window qualified as Window

{- | The 'Damageable' behavior — helpers that fire the standard window cascade
when an entity receives damage. Used by 'Arkham.Enemy.Runner' and may be
extended to anything else damageable (assets, locations, …).

The window order matches the canonical enemy-damage flow:

  1. @when WouldTakeDamage@
  2. @when DealtDamage@
  3. @after DealtDamage@
  4. @when TakeDamage@
  5. (caller's body: push the 'Damaged' message and any per-entity bookkeeping)
  6. @after TakeDamage@
-}

-- | Push the full canonical damage-window cascade in order, running the caller's
-- body between @when TakeDamage@ and @after TakeDamage@.
fireDamageWindows
  :: ReverseQueue m
  => Source -> Target -> DamageEffect -> Int -> m () -> m ()
fireDamageWindows source target damageEffect damageAmount body = do
  checkWhen $ Window.WouldTakeDamage source target damageAmount DamageDirect
  checkWhen $ Window.DealtDamage source damageEffect target damageAmount
  checkAfter $ Window.DealtDamage source damageEffect target damageAmount
  checkWhen $ Window.TakeDamage source damageEffect target damageAmount
  body
  checkAfter $ Window.TakeDamage source damageEffect target damageAmount
