module Arkham.Scenarios.ThreadsOfFate.Helpers where

import Arkham.Prelude

import Arkham.Act.Sequence qualified as Act
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey

getActDecksInPlayCount :: HasGame m => m Int
getActDecksInPlayCount = do
  hasDeckA <- selectAny $ ActOneOf [ActWithSide Act.A, ActWithSide Act.B]
  hasDeckC <- selectAny $ ActOneOf [ActWithSide Act.C, ActWithSide Act.D]
  hasDeckE <- selectAny $ ActOneOf [ActWithSide Act.E, ActWithSide Act.F]
  pure $ count id [hasDeckA, hasDeckC, hasDeckE]

isIchtacasPrey :: HasGame m => EnemyId -> m Bool
isIchtacasPrey eid = scenarioFieldMap ScenarioRemembered $ any $ \case
  IchtacasPrey (Labeled _ eid' `With` _) -> eid == eid'
  _ -> False

isIchtacasDestination :: HasGame m => LocationId -> m Bool
isIchtacasDestination lid = scenarioFieldMap ScenarioRemembered $ any $ \case
  IchtacasDestination (Labeled _ lid') -> lid == lid'
  _ -> False

rememberIchtacasPrey :: ReverseQueue m => EnemyId -> m ()
rememberIchtacasPrey eid = do
  card <- field EnemyCard eid
  remember $ IchtacasPrey $ Labeled (toName card) eid `With` Envelope @"cardCode" card.cardCode
