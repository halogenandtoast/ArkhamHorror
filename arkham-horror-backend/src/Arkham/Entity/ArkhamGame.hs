{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Entity.ArkhamGame where

import Arkham.Types.Game
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
ArkhamGame sql=arkham_games
  currentData ArkhamGameData
|]
