module Arkham.Scenarios.ThreadsOfFate.Helpers where

import Arkham.Prelude

import Arkham.Act.Sequence qualified as Act
import Arkham.Card
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Store

getActDecksInPlayCount :: (HasGame m, Store m Card) => m Int
getActDecksInPlayCount = do
  hasDeckA <- selectAny $ ActOneOf [ActWithSide Act.A, ActWithSide Act.B]
  hasDeckC <- selectAny $ ActOneOf [ActWithSide Act.C, ActWithSide Act.D]
  hasDeckE <- selectAny $ ActOneOf [ActWithSide Act.E, ActWithSide Act.F]
  pure $ count id [hasDeckA, hasDeckC, hasDeckE]

isIchtacasPrey :: (HasGame m, Store m Card) => EnemyId -> m Bool
isIchtacasPrey eid = scenarioFieldMap ScenarioRemembered $ any $ \case
  IchtacasPrey (Labeled _ eid') -> eid == eid'
  _ -> False

isIchtacasDestination :: (HasGame m, Store m Card) => LocationId -> m Bool
isIchtacasDestination lid = scenarioFieldMap ScenarioRemembered $ any $ \case
  IchtacasDestination (Labeled _ lid') -> lid == lid'
  _ -> False
