module Arkham.Helpers.Doom where

import Arkham.Prelude

import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Matcher
import Arkham.Target

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
