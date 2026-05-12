{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message.Defeat where

import Arkham.Card.Id
import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Data.Aeson.TH

-- | Messages dealing with entity defeat (enemies, assets, enemy-locations).
--
-- Re-exported by "Arkham.Message"; callers should use the bidirectional pattern
-- synonyms ('EnemyDefeated', 'DefeatEnemy', etc.) defined there.
-- The previously-Enemy-specific 'EnemyDefeated' constructor now lives here as
-- the generic 'Defeated' and routes by 'Target' so any entity can be defeated.
-- The generic name avoids the GADT Field constructor clash (@EnemyDefeated ::
-- Field Enemy Bool@) that prevented a back-compat pattern synonym.
data DefeatMessage
  = AssetDefeated_ Source AssetId
  | CheckDefeated_ Source Target
  | Defeated_ Target CardId Source [Trait]
  | DefeatEnemy_ EnemyId InvestigatorId Source
  | EnemyLocationDefeated_ LocationId CardId Source [Trait]
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''DefeatMessage)
