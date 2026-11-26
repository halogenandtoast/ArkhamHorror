{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

{- | The tracker: bugs and feature requests players file, and the board they are
worked on.

Everything a player files lives here rather than on GitHub, so a reporter does
not need an account anywhere else and can be answered where they filed. Three
tables:

* @reports@ is the report itself -- the opening post, who filed it, what kind of
  thing it is, and where it sits on the board.
* @report_events@ is the communication log: every reply and every move between
  columns, in one stream, so the page reads as a conversation rather than a
  record with a changelog beside it.
* @report_views@ is the last time each follower looked, which is the whole of
  what "unread" means. A row per viewer rather than one timestamp on the report,
  because a public tracker has no single reader to be behind.

@number@ is the report's public name, and it continues the numbering of the
GitHub issues that came before: issue #61 imported is report #61, and the next
thing filed is #62. That keeps every @#number@ already written into a commit
message pointing at the same thing it always did.

@kind@, @status@ and the event @kind@ are 'Text' rather than sum types, for the
reason @arkham_game_outcomes@ keeps its verdict as text: the set is checked at
the handler boundary, where a value outside it can be answered with a 400,
instead of failing to decode a row that is already in the database. The handler
owns those sets -- see "Base.Api.Handler.Reports".
-}
module Entity.Report where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.Arkham.Game
import Entity.User
import Orphans ()
import Relude

mkEntity
  $(discoverEntities)
  [persistLowerCase|
Report sql=reports
  Id UUID default=uuid_generate_v4()
  number Int
  kind Text
  title Text
  body Text
  status Text
  authorId UserId Maybe OnDeleteSetNull
  -- ^ Nulled rather than cascaded when an account goes: a tracker that forgets
  -- a report because its reporter left loses the bug, not just the name.
  authorName Text Maybe
  -- ^ Who filed it when that is not an account here -- the GitHub login on an
  -- imported issue. Read only when 'reportAuthorId' is empty, and never a claim
  -- that the name belongs to anybody on this site.
  blocking Bool default=False
  -- ^ The reporter cannot go on playing until it is fixed. Their claim, not a
  -- priority anyone else set.
  duplicateOfId ReportId Maybe OnDeleteSetNull
  hidden Bool default=False
  -- ^ Taken off the public list without being deleted, for whatever a public
  -- tracker turns out to attract. Still reachable by its author and by admins.
  gameId ArkhamGameId Maybe OnDeleteSetNull
  gameName Text Maybe
  -- ^ What the game was called when the report was filed, kept so the report
  -- still says what it was about once the game itself is gone.
  gameStep Int Maybe
  -- ^ The step the game was on at filing. Against the game's step now, this is
  -- how far the reporter has played past what they reported.
  exportUrl Text Maybe
  position Double default=0
  -- ^ Order within its board column. Rewritten 1..n for a whole column when
  -- anything in it moves, so there is no fractional arithmetic to run out of.
  createdAt UTCTime
  updatedAt UTCTime
  -- ^ Touched by anything that happens to the report, and what a viewer's
  -- 'reportViewViewedAt' is compared against to decide there is news.
  UniqueReportNumber number
  deriving Generic Show

ReportEvent sql=report_events
  Id UUID default=uuid_generate_v4()
  reportId ReportId OnDeleteCascade
  userId UserId Maybe OnDeleteSetNull
  authorName Text Maybe
  -- ^ As on 'Report': the name to show when the writer is not an account here.
  kind Text
  -- ^ Which of the two fields below to read: a @comment@ carries a body, a
  -- @status@ carries the column it moved to.
  body Text Maybe
  status Text Maybe
  createdAt UTCTime
  deriving Generic Show

ReportView sql=report_views
  Id UUID default=uuid_generate_v4()
  reportId ReportId OnDeleteCascade
  userId UserId OnDeleteCascade
  viewedAt UTCTime
  UniqueReportView reportId userId
  deriving Generic Show
|]
