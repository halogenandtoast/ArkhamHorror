{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | A third-edition table: a lobby of seats that becomes a game once the host
starts it. A table is one row, this whole record in its @payload@; see
"ThirdEdition.Store".
-}
module ThirdEdition.Table where

import AH3e.Game (Game)
import AH3e.Types.Card (Expansion)
import AH3e.Types.Ids (ScenarioCode)
import AH3e.Types.State (GameMode)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.!=), (.:?), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Model (UserId)
import Relude

-- | Seat @n@ is the engine's @PlayerId n@.
data Seat = Seat {player :: Int, user :: Maybe UserId, username :: Maybe Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TableOptions = TableOptions
  {expansions :: [Expansion], mode :: GameMode, scenario :: Maybe ScenarioCode}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Table = Table
  { id :: UUID
  , name :: Text
  , host :: UserId
  , hostName :: Text
  , seats :: [Seat]
  , options :: TableOptions
  , game :: Maybe Game
  , version :: Int
  -- ^ bumped on every write, so a client can drop an update older than what it shows
  , createdAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | A table as an export carries it: the table itself, plus the earlier game
states it can undo to.

The history is store state rather than table state, so it is not a 'Table'
field -- every live table's payload decodes with 'Table''s 'FromJSON', and a new
field there would strand the ones already written. Instead this flattens to the
very same object with one extra @history@ key, so @.game@ and the rest stay
where any existing reader expects them, and an older export without the key
still decodes.
-}
data TableExport = TableExport
  { table :: Table
  , history :: [Game]
  -- ^ newest first, the order the store reads them back in
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TableExport where
  toJSON e = case toJSON e.table of
    Object o -> Object (KeyMap.insert "history" (toJSON e.history) o)
    -- unreachable: Table is a record, so its encoding is an object
    v -> object ["table" .= v, "history" .= e.history]

instance FromJSON TableExport where
  parseJSON v = withObject "TableExport" (\o -> TableExport <$> parseJSON v <*> o .:? "history" .!= []) v

seatedUsers :: Table -> [UserId]
seatedUsers t = ordNub [u | Seat {user = Just u} <- t.seats]

isSeated :: UserId -> Table -> Bool
isSeated uid t = uid `elem` seatedUsers t

holdsSeat :: UserId -> Int -> Table -> Bool
holdsSeat uid n t = any (\s -> s.player == n && s.user == Just uid) t.seats

started :: Table -> Bool
started t = isJust t.game

bumpVersion :: Table -> Table
bumpVersion t = t {version = t.version + 1}

withGame :: Game -> Table -> Table
withGame g t = t {game = Just g}

withSeats :: [Seat] -> Table -> Table
withSeats ss t = t {seats = ss}

freeSeats :: Table -> [Int]
freeSeats t = [s.player | s <- t.seats, isNothing s.user]

-- | Take a seat for this user: the one asked for, or the first free one.
takeSeat :: UserId -> Text -> Maybe Int -> Table -> Either Text Table
takeSeat uid uname wanted t = case wanted of
  Just n | n `notElem` freeSeats t -> Left "That seat is taken"
  Just n -> Right (withSeats (map (sit n) t.seats) t)
  Nothing -> case freeSeats t of
    [] -> Left "No free seats"
    n : _ -> Right (withSeats (map (sit n) t.seats) t)
 where
  sit n s = if s.player == n then s {user = Just uid, username = Just uname} else s

-- | Give up one seat, or every seat this user holds.
leaveSeat :: UserId -> Maybe Int -> Table -> Table
leaveSeat uid wanted t = withSeats (map vacate t.seats) t
 where
  vacate s
    | s.user == Just uid && maybe True (== s.player) wanted = s {user = Nothing, username = Nothing}
    | otherwise = s
