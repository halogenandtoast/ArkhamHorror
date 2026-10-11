{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.GameArchive (
  module Entity.Arkham.GameArchive,
) where

import Data.Text qualified as T
import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Entity
import Entity.Arkham.Game
import Entity.User
import Json
import Orphans ()
import Relude

{- | Where a game's undo history is.

'Archived' means the steps are in object storage and not in Postgres, so undo
cannot work until they come back. 'Restoring' is the window between a player
asking for them and the download finishing -- short, but long enough that two
people pressing undo must not start two downloads. 'Restored' is the resting
state of a game that was archived once and is playable again; it is kept rather
than deleted so the panel can still say what the archive reclaimed, and so a
game that goes idle again is recognisably a repeat.

'Failed' is a restore that could not be completed. It is deliberately not
'Archived': the object is still there, but something about it did not work, and
retrying on a loop would just fail on a loop. It wants a person.
-}
data ArchiveState = Archived | Restoring | Restored | Failed
  deriving stock (Show, Eq, Generic)

{- | Lowercase in JSON as well as in the column, so the string a client compares
against is the string stored, and both match the error codes the undo
endpoints refuse with.
-}
instance ToJSON ArchiveState where
  toJSON = toJSON . T.toLower . T.pack . show

instance FromJSON ArchiveState where
  parseJSON = withText "ArchiveState" \case
    "archived" -> pure Archived
    "restoring" -> pure Restoring
    "restored" -> pure Restored
    "failed" -> pure Failed
    other -> fail $ "invalid archive state: " <> toString other

instance PersistField ArchiveState where
  toPersistValue = PersistText . T.toLower . T.pack . show
  fromPersistValue (PersistText "archived") = Right Archived
  fromPersistValue (PersistText "restoring") = Right Restoring
  fromPersistValue (PersistText "restored") = Right Restored
  fromPersistValue (PersistText "failed") = Right Failed
  fromPersistValue _ = Left "invalid archive state"

instance PersistFieldSql ArchiveState where
  sqlType _ = SqlString

{- | One row per game, written when its undo history is offloaded and updated in
place when it comes back. See the @arkham_game_archives@ migration for why the
shape is this and not one row per archive.

@steps@ and @bytes@ outlive a restore on purpose: they are the only record of
what the sweep actually reclaimed.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamGameArchive sql=arkham_game_archives
  Id UUID default=uuid_generate_v4()
  arkhamGameId ArkhamGameId OnDeleteCascade
  state ArchiveState
  bucket Text
  objectKey Text
  steps Int
  topStep Int
  bytes Int
  archivedAt UTCTime
  gameUpdatedAt UTCTime
  requestedAt UTCTime Maybe
  requestedBy UserId Maybe
  restoredAt UTCTime Maybe
  errorMessage Text Maybe
  UniqueGameArchive arkhamGameId
  deriving Generic Show
|]

instance ToJSON ArkhamGameArchive where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamGameArchive"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamGameArchive"
