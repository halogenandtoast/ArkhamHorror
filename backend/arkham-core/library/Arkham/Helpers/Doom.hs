module Arkham.Helpers.Doom where

import Arkham.Prelude

import Arkham.Agenda.Types
import Arkham.Asset.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Target
import Arkham.Treachery.Types

targetsWithDoom :: HasGame m => m [Target]
targetsWithDoom = do
  locations <- selectMap LocationTarget LocationWithAnyDoom
  investigators <- selectMap InvestigatorTarget InvestigatorWithAnyDoom
  enemies <- selectMap EnemyTarget EnemyWithAnyDoom
  assets <- selectMap AssetTarget AssetWithAnyDoom
  agendas <- selectMap AgendaTarget AgendaWithAnyDoom
  treacheries <- selectMap TreacheryTarget TreacheryWithAnyDoom
  -- acts <- selectMap ActTarget ActWithAnyDoom
  -- events <- selectMap EventTarget EventWithAnyDoom
  -- skills <- selectMap SkillTarget SkillWithAnyDoom
  pure
    $ locations
    <> investigators
    <> enemies
    <> assets
    <> agendas
    <> treacheries

getDoomCount :: HasGame m => m Int
getDoomCount =
  getSum
    . fold
    <$> sequence
      [ selectAgg Sum AssetDoom AnyAsset
      , selectAgg Sum EnemyDoom AnyEnemy
      , selectAgg Sum LocationDoom Anywhere
      , selectAgg Sum TreacheryDoom AnyTreachery
      , selectAgg Sum AgendaDoom AnyAgenda
      , selectAgg Sum InvestigatorDoom UneliminatedInvestigator
      ]
