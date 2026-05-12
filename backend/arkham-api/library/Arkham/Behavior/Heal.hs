module Arkham.Behavior.Heal where

import Arkham.Classes.HasGame (HasGame)
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Damage (DamageType)
import Arkham.Helpers.Message (push)
import Arkham.Helpers.Window (checkAfter, checkWhen)
import Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Source (Source)
import Arkham.Target (Target)
import Arkham.Window qualified as Window

{- | The 'Healable' behavior — helpers that fire the standard window cascade
when an entity is healed. Used by entity runners that respond to
'HealDamage' / 'HealHorror' / 'HealAllDamage' / 'HealAllHorror' /
'HealAllDamageAndHorror' messages.

The canonical heal cascade is just an @after Window.Healed@ window. Some
hosts (notably 'Arkham.Investigator.Runner') additionally fire the matching
@when@ window before applying the heal; that's exposed via
'fireHealWindows'.
-}

-- | Push the standard @after Window.Healed@ window for an entity. Caller is
-- responsible for the actual token / state mutation.
pushHealedAfter
  :: (HasGame m, HasQueue Message m)
  => DamageType -> Target -> Source -> Int -> m ()
pushHealedAfter damageType target source n = do
  afterWindow <- checkAfter $ Window.Healed damageType target source n
  push afterWindow

-- | Fire both @when@ and @after Window.Healed@ around the caller's body —
-- used by hosts that need a pre-heal hook (Investigator).
fireHealWindows
  :: (HasGame m, HasQueue Message m)
  => DamageType -> Target -> Source -> Int -> m () -> m ()
fireHealWindows damageType target source n body = do
  whenWindow <- checkWhen $ Window.Healed damageType target source n
  push whenWindow
  body
  afterWindow <- checkAfter $ Window.Healed damageType target source n
  push afterWindow
