module Arkham.Scenarios.ThreadsOfFate.Helpers where

import Arkham.Prelude

import Arkham.Act.Sequence qualified as Act
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Name
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

getIchtacasPrey :: HasGame m => m [EnemyId]
getIchtacasPrey = do
  remembered <- scenarioField ScenarioRemembered
  pure $ flip mapMaybe (toList remembered) \case
    IchtacasPrey (Labeled _ eid' `With` _) -> Just eid'
    _ -> Nothing

isIchtacasDestination :: HasGame m => LocationId -> m Bool
isIchtacasDestination lid = scenarioFieldMap ScenarioRemembered $ any $ \case
  IchtacasDestination (Labeled _ lid') -> lid == lid'
  _ -> False

rememberIchtacasPrey :: ReverseQueue m => EnemyId -> Card -> m ()
rememberIchtacasPrey eid card = remember $ IchtacasPrey $ Labeled (toName card) eid `With` Envelope @"cardCode" card.cardCode

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "threadsOfFate" a
