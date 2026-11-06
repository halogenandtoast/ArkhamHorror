module Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons.Helpers where

import Arkham.ScenarioLogKey

threwAChairThroughTheConservatoryWindow :: ScenarioLogKey
threwAChairThroughTheConservatoryWindow =
  HomebrewScenarioLogKey "nightOfVespers.ThrewAChairThroughTheConservatoryWindow"

foundAWayThroughTheHedgeMaze :: ScenarioLogKey
foundAWayThroughTheHedgeMaze =
  HomebrewScenarioLogKey "nightOfVespers.FoundAWayThroughTheHedgeMaze"

forcedOpenTheRooftopSkylight :: ScenarioLogKey
forcedOpenTheRooftopSkylight =
  HomebrewScenarioLogKey "nightOfVespers.ForcedOpenTheRooftopSkylight"

escapeRoutes :: [ScenarioLogKey]
escapeRoutes =
  [ threwAChairThroughTheConservatoryWindow
  , foundAWayThroughTheHedgeMaze
  , forcedOpenTheRooftopSkylight
  ]
