{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.Achievement (
  module Entity.Arkham.Achievement,
) where

import Arkham.Achievement.Types (Achievement)
import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.Arkham.Game
import Entity.User
import Json
import Orphans ()
import Relude

-- One row per user per achievement, forever. `arkhamGameId` links back to the
-- earning game and degrades to NULL when that game is deleted. `progress`
-- carries counter/checklist state for cross-playthrough achievements while
-- `earnedAt` is still null.
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamAchievement sql=arkham_achievements
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  achievement Achievement
  earnedAt UTCTime Maybe
  arkhamGameId ArkhamGameId Maybe OnDeleteSetNull
  progress Value
  UniqueUserAchievement userId achievement
  deriving Generic Show
|]

instance ToJSON ArkhamAchievement where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamAchievement"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamAchievement"
