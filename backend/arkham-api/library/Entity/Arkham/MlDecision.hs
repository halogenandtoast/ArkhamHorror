{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Persistence for LIVE imitation-learning data capture.
--
-- One 'ArkhamMlDecision' row is written per HUMAN, multi-choice decision as it
-- happens on the CURRENT engine (see
-- 'Api.Handler.Arkham.Games.Shared.captureMlDecision'). Because the row is
-- written against the live engine — not replayed from a historical dump — it is
-- drift-free, and the chosen index is a direct label (no re-execution needed).
--
-- @rows@ is a denormalized jsonb array, one element per flattened choice:
--
-- @
--   [ { "chosen": Bool, "features": { 67 keys }, "breakdown": { 10 terms } } ]
-- @
--
-- The standalone @arkham-ml-export@ executable later unpacks each element into
-- one JSONL line for @ml/train.py@.
module Entity.Arkham.MlDecision (
  module Entity.Arkham.MlDecision,
) where

import Data.Aeson.Types
import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity
import Entity.Arkham.Game
import Json
import Orphans ()
import Relude

mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamMlDecision sql=arkham_ml_decisions
  Id UUID default=uuid_generate_v4()
  arkhamGameId ArkhamGameId OnDeleteCascade
  step Int
  playerId Text
  chosenIndex Int
  rows Value
  createdAt UTCTime
  deriving Generic Show
|]

instance ToJSON ArkhamMlDecision where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamMlDecision"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamMlDecision"
