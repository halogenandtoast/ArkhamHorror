module Arkham.Helpers.Doom where

import Arkham.Prelude

import Arkham.Agenda.Types
import Arkham.Asset.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types
import Arkham.Event.Types
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Investigator.Types
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Types

targetsWithDoom :: HasGame m => m [Target]
targetsWithDoom = do
  locations <- selectTargets LocationWithAnyDoom
  investigators <- selectTargets InvestigatorWithAnyDoom
  enemies <- selectTargets EnemyWithAnyDoom
  events <- selectTargets EventWithAnyDoom
  assets <- selectTargets AssetWithAnyDoom
  agendas <- selectTargets AgendaWithAnyDoom
  treacheries <- selectTargets TreacheryWithAnyDoom
  -- acts <- selectMap ActTarget ActWithAnyDoom
  -- skills <- selectMap SkillTarget SkillWithAnyDoom
  pure
    $ locations
    <> investigators
    <> enemies
    <> events
    <> assets
    <> agendas
    <> treacheries

getDoomOnTarget :: HasGame m => Target -> m Int
getDoomOnTarget = \case
  AssetTarget aid -> field AssetDoom aid
  InvestigatorTarget iid -> field InvestigatorDoom iid
  EnemyTarget eid -> field EnemyDoom eid
  LocationTarget lid -> field LocationDoom lid
  TreacheryTarget lid -> field TreacheryDoom lid
  AgendaTarget lid -> field AgendaDoom lid
  EventTarget lid -> field EventDoom lid
  _ -> pure 0

getDoomCount :: HasGame m => m Int
getDoomCount = do
  adds <-
    getSum
      . fold
      <$> sequence
        [ selectAgg Sum AssetDoom (AssetWithoutModifier DoomSubtracts)
        , selectAgg Sum EnemyDoom (EnemyWithoutModifier DoomSubtracts)
        , selectAgg Sum EventDoom (EventWithoutModifier DoomSubtracts)
        , selectAgg Sum LocationDoom (LocationWithoutModifier DoomSubtracts)
        , selectAgg Sum TreacheryDoom (TreacheryWithoutModifier DoomSubtracts)
        , selectAgg Sum AgendaDoom (AgendaWithoutModifier DoomSubtracts)
        , selectAgg
            Sum
            InvestigatorDoom
            (UneliminatedInvestigator <> InvestigatorWithoutModifier DoomSubtracts)
        ]
  addDoomAssets <- select $ AssetWithAnyDoom <> AssetWithoutModifier DoomSubtracts
  ignoredDoomAdd <-
    sum <$> for addDoomAssets \aid -> do
      doom <- field AssetDoom aid
      mods <- getModifiers aid
      pure $ sum $ flip mapMaybe mods \case
        IgnoreDoomOnThis n -> Just (min n doom)
        _ -> Nothing

  subtracts <-
    getSum
      . fold
      <$> sequence
        [ selectAgg Sum AssetDoom (AssetWithModifier DoomSubtracts)
        , selectAgg Sum EnemyDoom (EnemyWithModifier DoomSubtracts)
        , selectAgg Sum EventDoom (EventWithModifier DoomSubtracts)
        , selectAgg Sum LocationDoom (LocationWithModifier DoomSubtracts)
        , selectAgg Sum TreacheryDoom (TreacheryWithModifier DoomSubtracts)
        , selectAgg Sum AgendaDoom (AgendaWithModifier DoomSubtracts)
        , selectAgg
            Sum
            InvestigatorDoom
            (UneliminatedInvestigator <> InvestigatorWithModifier DoomSubtracts)
        ]

  subtractDoomAssets <- select $ AssetWithAnyDoom <> AssetWithModifier DoomSubtracts
  ignoredDoomSubtract <-
    sum <$> for subtractDoomAssets \aid -> do
      doom <- field AssetDoom aid
      mods <- getModifiers aid
      pure $ sum $ flip mapMaybe mods \case
        IgnoreDoomOnThis n -> Just (min n doom)
        _ -> Nothing
  pure $ max 0 ((adds - ignoredDoomAdd) - (subtracts + ignoredDoomSubtract))
