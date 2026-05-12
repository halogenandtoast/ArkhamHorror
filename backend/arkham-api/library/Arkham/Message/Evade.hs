{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Evade where

import Arkham.Evade.Types
import Arkham.Id
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages addressed to anything that can be evaded, or to the act of evading.
--
-- The internal constructors take 'Target' rather than 'EnemyId' so any evadable
-- entity can receive them. "Arkham.Message" exposes two public forms per
-- constructor:
--
--   * Generic forms ('TryEvadeTarget', 'EvadeTarget', 'EvadedTarget',
--     'ChosenEvadeTarget', 'AfterEvadeTarget') match any 'Target'.
--   * Back-compat forms ('TryEvadeEnemy', 'EvadeEnemy', 'EnemyEvaded',
--     'ChosenEvadeEnemy', 'AfterEvadeEnemy') match only an 'EnemyTarget',
--     preserving the pre-rename public API.
data EvadeMessage
  = ChooseEvadeEnemy_ ChooseEvade
  | TryEvadeTarget_ SkillTestId InvestigatorId Target Source (Maybe Target) SkillType
  | EvadeTarget_ SkillTestId InvestigatorId Target Source (Maybe Target) SkillType Bool
  | EvadedTarget_ InvestigatorId Target
  | ChosenEvadeTarget_ SkillTestId Source Target
  | AfterEvadeTarget_ InvestigatorId Target
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''EvadeMessage)
