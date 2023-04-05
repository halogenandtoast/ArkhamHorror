{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Game.Json where

import Arkham.Prelude

import Arkham.Game.Base
import Data.Aeson.TH

-- being json instances into scope
import Arkham.Investigator ()
import Arkham.Entities ()
import Arkham.Campaign ()
import Arkham.Scenario ()

$(deriveJSON defaultOptions ''GameState)
$(deriveJSON defaultOptions ''GameParams)
$(deriveJSON defaultOptions ''Game)
