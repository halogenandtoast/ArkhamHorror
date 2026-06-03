{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Fight where

import Arkham.Fight.Types
import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Data.Aeson.TH

-- | Messages addressed to anything that can be fought, or to the act of fighting.
--
-- The internal constructors take 'Target' rather than 'EnemyId' so the same
-- messages can address any entity that is fightable (enemies, enemy-locations,
-- pylons, etc.). Call sites use the bidirectional pattern synonyms in
-- "Arkham.Message":
--
--   * Generic forms ('FightTarget', 'AttackTarget', 'FailedAttackTarget')
--     match any 'Target'.
--   * Back-compat forms ('FightEnemy', 'AttackEnemy', 'FailedAttackEnemy')
--     match only an 'EnemyTarget', preserving the pre-rename public API.
data FightMessage
  = FightTarget_ Target ChooseFight
  | AttackTarget_ Target ChooseFight
  | FailedAttackTarget_ InvestigatorId Target
  | ChooseFightEnemy_ ChooseFight
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''FightMessage)
