{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Entity.ArkhamGame
  ( ArkhamGame(..)
  , ArkhamGameId
  )
where

import Arkham.Types
import Arkham.Types.Game
import Base.Lock
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import Database.Persist.TH
import Lens.Micro

mkPersist sqlSettings [persistLowerCase|
ArkhamGame sql=arkham_games
  currentData ArkhamGameData
  deriving Generic Show
|]

instance ToJSON ArkhamGame where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 10 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 10 }

instance HasLock ArkhamGame where
  type LockKey ArkhamGame = String
  lock = currentData . lock

instance HasCurrentData ArkhamGame where
  currentData =
    lens arkhamGameCurrentData (\m x -> m { arkhamGameCurrentData = x })

instance HasChaosBag ArkhamGame where
  chaosBag = currentData . chaosBag

instance HasLocations ArkhamGame where
  locations = currentData . locations

instance HasGameStateStep ArkhamGame where
  gameStateStep = currentData . gameStateStep

instance HasPlayer ArkhamGame where
  player = currentData . player

instance HasScenario ArkhamGame where
  scenario = currentData . scenario

instance HasDifficulty ArkhamGame where
  difficulty = currentData . difficulty

instance HasPhase ArkhamGame where
  phase = currentData . phase

instance HasStacks ArkhamGame where
  stacks = currentData . stacks
