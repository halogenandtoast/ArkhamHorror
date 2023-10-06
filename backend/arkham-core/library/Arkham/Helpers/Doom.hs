module Arkham.Helpers.Doom where

import Arkham.Prelude

import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Matcher
import Arkham.Target

targetsWithDoom :: HasGame m => m [Target]
targetsWithDoom = do
  locations <- selectListMap LocationTarget LocationWithAnyDoom
  investigators <- selectListMap InvestigatorTarget InvestigatorWithAnyDoom
  enemies <- selectListMap EnemyTarget EnemyWithAnyDoom
  assets <- selectListMap AssetTarget AssetWithAnyDoom
  agendas <- selectListMap AgendaTarget AgendaWithAnyDoom
  treacheries <- selectListMap TreacheryTarget TreacheryWithAnyDoom
  -- acts <- selectListMap ActTarget ActWithAnyDoom
  -- events <- selectListMap EventTarget EventWithAnyDoom
  -- skills <- selectListMap SkillTarget SkillWithAnyDoom
  pure
    $ locations
    <> investigators
    <> enemies
    <> assets
    <> agendas
    <> treacheries
