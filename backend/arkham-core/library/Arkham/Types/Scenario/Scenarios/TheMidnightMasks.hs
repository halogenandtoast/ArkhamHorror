{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import ClassyPrelude

newtype TheMidnightMasks = TheMidnightMasks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks =
  TheMidnightMasks . baseAttrs "01120" "The Midnight Masks" [] []

instance (ScenarioRunner env) => RunMessage env TheMidnightMasks where
  runMessage msg (TheMidnightMasks attrs) =
    TheMidnightMasks <$> runMessage msg attrs
