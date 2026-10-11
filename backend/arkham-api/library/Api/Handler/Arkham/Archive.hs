{- | What a player sees of the archive: "your undo history is in cold storage",
and the button that asks for it back.

Undo is the only thing an archived game cannot do, so this is deliberately lazy:
nothing warns anyone in advance, and the first press of Undo is what surfaces it.
The alternative -- carrying an archived flag in the game payload so the client
could grey the button out -- means every game update on every socket carries a
field that is false for all but a handful of games.
-}
module Api.Handler.Arkham.Archive (
  getApiV1ArkhamGameArchiveR,
  putApiV1ArkhamGameUnarchiveR,
  checkArchiveSeam,
  isArchiveRefusal,
) where

import Api.Arkham.Archive
import Data.Aeson.KeyMap qualified as KM
import Data.Time.Clock
import Database.Persist qualified as P
import Entity.Arkham.Player
import Import hiding ((==.))
import Json

{- | The archive as the client needs it.

@state@ is the lifecycle value, so the client can tell "offer to restore this"
from "a restore is already running" without a second round trip.
-}
archiveJson :: ArkhamGameArchive -> Json.Value
archiveJson archive =
  object
    [ "state" .= archive.state
    , "steps" .= archive.steps
    , "bytes" .= archive.bytes
    , "archivedAt" .= archive.archivedAt
    , "requestedAt" .= archive.requestedAt
    , "restoredAt" .= archive.restoredAt
    , "error" .= archive.errorMessage
    ]

{- | Whether an undo can go as far back as it wants to, given the archive.

Archiving a game does not stop it being played, so a game can sit above its own
seam: steps 0..@topStep@ are in the bucket, and everything the player has done
since is in the database as usual. An undo that only needs those later steps is
an ordinary undo and must not be interrupted -- so the question is never "is this
game archived" but "does this particular undo reach below the seam".

@lowestStepNeeded@ is the lowest step row the undo has to read: @n - 1@ for a
single step (it reverts @n@ and lands on @n - 1@, which it checks exists), and
@toStep + 1@ for a multi-step undo (it reverts everything above @toStep@).

This is a correctness check before it is a courtesy. A multi-step undo past the
seam would find only the rows that survived, apply that partial set of inverse
patches, and leave the game somewhere that never existed.

'Restored' is not consulted: the rows are back, and the row is only still there
so the panel can say what the archive once reclaimed.
-}
archiveRefusal :: Int -> ArkhamGameArchive -> Maybe Json.Value
archiveRefusal lowestStepNeeded archive = case archive.state of
  Restored -> Nothing
  _
    | lowestStepNeeded > archive.topStep -> Nothing
    | otherwise -> Just $ object ["error" .= code, "archive" .= archiveJson archive]
 where
  code :: Text
  code = case archive.state of
    Archived -> "archived"
    Restoring -> "restoring"
    Failed -> "restoreFailed"
    Restored -> "archived"

{- | 'archiveRefusal' against this game's archive, if it has one.

A unique-index lookup on the undo path, replacing the blanket pre-check that
used to refuse every undo on an archived game.
-}
checkArchiveSeam :: Int -> ArkhamGameId -> DB (Maybe Json.Value)
checkArchiveSeam lowestStepNeeded gameId =
  (>>= archiveRefusal lowestStepNeeded) <$> lookupArchive gameId

{- | Whether an undo's error payload is one of these refusals.

The undo handlers answer a refused undo with the status this picks, and nothing
else distinguishes it: those functions return a bare 'Json.Value' for every
failure, so this reads the code back out rather than changing seven signatures
to carry a type that only two call sites look at.
-}
isArchiveRefusal :: Json.Value -> Bool
isArchiveRefusal = \case
  Object o -> case KM.lookup "error" o of
    Just (String code) -> code `elem` (["archived", "restoring", "restoreFailed"] :: [Text])
    _ -> False
  _ -> False

{- | Where this game's undo history is. @null@ for a game that has never been
archived, which is almost all of them.
-}
getApiV1ArkhamGameArchiveR :: ArkhamGameId -> Handler Json.Value
getApiV1ArkhamGameArchiveR gameId = do
  _ <- getRequestUserId
  mArchive <- runDB $ lookupArchive gameId
  pure $ maybe Json.Null archiveJson mArchive

{- | Ask for a game's undo history back.

Returns as soon as the download is claimed, not when it has finished: the file
is a gzipped export of every step the game ever took, and the player is holding
a button. 'startRestore' publishes the outcome to the game's room, which is where
the client is already listening.

Anyone at the table may ask. It is their history, it restores to the state the
game is already in, and the only cost of being wrong is some bandwidth.
-}
putApiV1ArkhamGameUnarchiveR :: ArkhamGameId -> Handler Json.Value
putApiV1ArkhamGameUnarchiveR gameId = do
  -- Unset bucket means the mechanism does not exist on this deployment; nothing
  -- can have been archived, so there is nothing here to ask for.
  settings <- getsApp appSettings
  _ <- maybe notFound pure (archiveConfig settings)

  Entity userId user <- getRequestUser
  unless user.admin do
    mSeat <- runDB $ P.getBy (UniquePlayer userId gameId)
    when (isNothing mSeat) $ permissionDenied "You are not in this game"

  now <- liftIO getCurrentTime
  mArchive <- runDB $ lookupArchive gameId
  case mArchive of
    Nothing -> notFound
    Just archive -> case archive.state of
      Restored -> pure $ object ["state" .= Restored, "alreadyRestored" .= True]
      Restoring -> pure $ object ["state" .= Restoring, "alreadyRunning" .= True]
      _ -> do
        mClaimed <- runDB $ claimForRestore now (Just userId) gameId
        case mClaimed of
          -- Someone else claimed it between the read and the write, which is
          -- the same answer from the player's point of view: it is coming.
          Nothing -> pure $ object ["state" .= Restoring, "alreadyRunning" .= True]
          Just claimed -> do
            app <- getApp
            liftIO $ startRestore app claimed
            pure $ object ["state" .= Restoring, "steps" .= claimed.steps]
