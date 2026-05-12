{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Damage where

import Arkham.DamageEffect (DamageAssignment)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages dealing damage to or healing damage on game entities.
--
-- Re-exported by "Arkham.Message"; callers should use the bidirectional
-- pattern synonyms ('EnemyDamage', 'HealDamage', etc.) defined there rather
-- than the underscore-suffixed inner constructors below.
-- The previously-Enemy-specific 'EnemyDamage' and 'EnemyDamaged' constructors
-- now live here as 'DealDamage' and 'DealtDamage', and route by 'Target' so
-- the same messages can damage any entity (enemies, enemy-locations, ...).
-- The generic names avoid the GADT Field constructor clash (@EnemyDamage ::
-- Field Enemy Int@) that prevented back-compat pattern synonyms.
data DamageMessage
  = DealDamage_ Target DamageAssignment
  | Damaged_ Target DamageAssignment
  | HealDamage_ Target Source Int
  | HealAllDamage_ Target Source
  | PlaceAdditionalDamage_ Target Source Int Int
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''DamageMessage)
