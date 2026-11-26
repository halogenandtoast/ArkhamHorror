{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

{- | An admin-authored announcement, which the home page shows to everybody.

@category@ is what kind of announcement it is -- @important@, @feature@ or
@content@ -- and is what the home page stamps it with. NULL is an ordinary
announcement and carries no stamp, which is also what every row written before
the column existed is. Text rather than a sum type for the same reason
@arkham_game_outcomes@ keeps its verdict as text: the set is checked at the
handler boundary, where a value outside it can be answered with a 400 instead
of failing to decode a row that is already in the database.
-}
module Entity.Notification where

import Database.Persist.TH
import Relude
import Data.Time.Clock

mkPersist
  sqlSettings
  [persistLowerCase|
Notification json sql=notifications
  body Text
  category Text Maybe
  createdAt UTCTime
  deriving Show
|]
