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
  3. @when TakeDamage@
  4. (caller's body: push the 'Damaged' message and any per-entity bookkeeping)
  5. @after DealtDamage@
  6. @after TakeDamage@

Both @after@ windows come after the body because defeat is part of *dealing* damage
(Rules Reference, "Dealing Damage/Horror" step 2) and @after...@ effects only execute
once the triggering condition has fully resolved (FAQ 1.4, "Nested Sequences"). The
body's 'Damaged' pushes @AssignedDamage@ + @checkDefeated@ ahead of them, so a lethal
hit has already defeated and discarded the enemy by the time either window opens, #5682.
-}

{- | Push the full canonical damage-window cascade in order, running the caller's
body between @when TakeDamage@ and @after DealtDamage@.
-}
fireDamageWindows
  :: ReverseQueue m
  => Source -> Target -> DamageEffect -> Int -> m () -> m ()
fireDamageWindows source target damageEffect damageAmount body = do
  checkWhen $ Window.WouldTakeDamage source target damageAmount DamageDirect
  checkWhen $ Window.DealtDamage source damageEffect target damageAmount
  checkWhen $ Window.TakeDamage source damageEffect target damageAmount
  body
  checkAfter $ Window.DealtDamage source damageEffect target damageAmount
  checkAfter $ Window.TakeDamage source damageEffect target damageAmount
