{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.GameOutcome (
  module Entity.Arkham.GameOutcome,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.Arkham.Game
import Json
import Orphans ()
import Relude

{- | How a finished game ended, for games the engine did not record it for.

One row per game, and only for a game whose verdict had to be worked out after
the fact; a game the engine recorded an outcome for carries it in
@current_data@ instead, and that one wins wherever both exist. See the
@arkham_game_outcomes@ migration for why this is a side table rather than a
write into the game blob.

@certainty@ is @Uncertain@ for an ending the Campaign Guide prints as "The
investigators win the campaign...?", and @note@ is an i18n key for a clause it
attaches to the verdict; both mirror "Arkham.Game.Outcome".

@source@ is always @Inferred@ today. It is stored rather than assumed so that a
row whose verdict was reconstructed stays distinguishable from one that was not,
if anything ever writes a recorded outcome here.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamGameOutcome sql=arkham_game_outcomes
  Id UUID default=uuid_generate_v4()
  arkhamGameId ArkhamGameId OnDeleteCascade
  result Text
  certainty Text
  note Text Maybe
  source Text
  scenarioId Text Maybe
  resolutionTag Text Maybe
  resolution Int Maybe
  computedAt UTCTime
  UniqueGameOutcome arkhamGameId
  deriving Generic Show
|]

instance ToJSON ArkhamGameOutcome where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamGameOutcome"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamGameOutcome"
