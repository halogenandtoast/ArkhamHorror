{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan @Ord@/@Data@ instances for the AI foundation types.
--
-- 'Arkham.Message.Message' derives @(Show, Eq, Ord, Data)@, and
-- 'Arkham.Game.Settings.Settings' derives @Data@. To carry 'AiPlayerState'
-- and 'Focus' inside a 'Message' constructor (and 'AiPlayerState' inside
-- 'Settings') those payloads must therefore be @Ord@ and @Data@. The
-- foundation modules @Arkham.Ai.State@ / @Arkham.Ai.Focus@ are owned
-- elsewhere and only derive @(Show, Eq[, Ord])@, so the missing instances are
-- supplied here as orphans rather than by editing the foundation.
--
-- Import this module (qualified or with @()@) wherever those derived
-- instances are needed: 'Arkham.Message', 'Arkham.Game.Settings'.
module Arkham.Ai.Orphans () where

import Arkham.Ai.Focus (Focus (..))
import Arkham.Ai.State (AiPlayerState (..))
import Arkham.Prelude

deriving stock instance Data Focus

deriving stock instance Ord AiPlayerState

deriving stock instance Data AiPlayerState
