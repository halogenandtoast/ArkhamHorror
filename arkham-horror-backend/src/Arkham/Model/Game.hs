{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE UndecidableInstances            #-}
module Arkham.Model.Game where

import Data.Text
import Database.Persist.Sql
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
ArkhamHorrorGame json sql=arkham_horror_games
  cycleId Text
  scenarioId Text
|]
