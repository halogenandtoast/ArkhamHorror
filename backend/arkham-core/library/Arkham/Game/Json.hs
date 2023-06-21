{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game.Json where

import Arkham.Prelude

import Arkham.Game.Base
import Data.Aeson.TH

-- being json instances into scope

import Arkham.Campaign ()
import Arkham.Entities ()
import Arkham.Investigator ()
import Arkham.Scenario ()

$(deriveJSON defaultOptions ''GameState)
$(deriveJSON defaultOptions ''Game)
