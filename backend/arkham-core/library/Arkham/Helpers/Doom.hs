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
import Arkham.Modifier
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
getDoomCount = do
  adds <-
    getSum
      . fold
      <$> sequence
        [ selectAgg Sum AssetDoom (AssetWithoutModifier DoomSubtracts)
        , selectAgg Sum EnemyDoom (EnemyWithoutModifier DoomSubtracts)
        , selectAgg Sum LocationDoom (LocationWithoutModifier DoomSubtracts)
        , selectAgg Sum TreacheryDoom (TreacheryWithoutModifier DoomSubtracts)
        , selectAgg Sum AgendaDoom (AgendaWithoutModifier DoomSubtracts)
        , selectAgg
            Sum
            InvestigatorDoom
            (UneliminatedInvestigator <> InvestigatorWithoutModifier DoomSubtracts)
        ]

  subtracts <-
    getSum
      . fold
      <$> sequence
        [ selectAgg Sum AssetDoom (AssetWithModifier DoomSubtracts)
        , selectAgg Sum EnemyDoom (EnemyWithModifier DoomSubtracts)
        , selectAgg Sum LocationDoom (LocationWithModifier DoomSubtracts)
        , selectAgg Sum TreacheryDoom (TreacheryWithModifier DoomSubtracts)
        , selectAgg Sum AgendaDoom (AgendaWithModifier DoomSubtracts)
        , selectAgg
            Sum
            InvestigatorDoom
            (UneliminatedInvestigator <> InvestigatorWithModifier DoomSubtracts)
        ]
  pure $ max 0 (adds - subtracts)
