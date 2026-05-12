{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.EnemyAttack where

import Arkham.Attack.Types
import Arkham.Card
import Arkham.Id
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

-- | Messages that originate from / coordinate an enemy attack.
--
-- Re-exported by "Arkham.Message"; call sites should refer to the bidirectional
-- pattern synonyms in "Arkham.Message" (e.g. 'EnemyAttack', 'InitiateEnemyAttack',
-- 'PerformEnemyAttack', 'EnemiesAttack', etc.) rather than the raw constructors
-- below. The trailing underscore is what lets the pattern synonyms preserve the
-- pre-extraction public names.
--
-- Note: the wrapper type is named 'EnemyAttackMessage' rather than 'AttackMessage'
-- because 'AttackMessage' already exists as a value in "Arkham.Message.Type".
data EnemyAttackMessage
  = EnemiesAttack_
  | EnemyWillAttack_ EnemyAttackDetails
  | EnemyAttack_ EnemyAttackDetails
  | InitiateEnemyAttack_ EnemyAttackDetails
  | PerformEnemyAttack_ EnemyId
  | AfterEnemyAttack_ EnemyId [Message]
  | EnemyAttackFromDiscard_ InvestigatorId Source Card
  | EnemyAttackIfEngaged_ EnemyId (Maybe InvestigatorId)
  | EnemyAttacks_ [Message]
  | ChangeEnemyAttackTarget_ EnemyId Target
  | ChangeEnemyAttackDetails_ EnemyId EnemyAttackDetails
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''EnemyAttackMessage)
