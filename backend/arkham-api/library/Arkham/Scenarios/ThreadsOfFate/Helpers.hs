module Arkham.Scenarios.ThreadsOfFate.Helpers where

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
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Name
import Arkham.Prelude
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Source

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
  xs <- scenarioField ScenarioRemembered
  pure $ flip mapMaybe (toList xs) \case
    IchtacasPrey (Labeled _ eid' `With` _) -> Just eid'
    _ -> Nothing

isIchtacasDestination :: HasGame m => LocationId -> m Bool
isIchtacasDestination lid = scenarioFieldMap ScenarioRemembered $ any $ \case
  IchtacasDestination (Labeled _ lid' `With` _) -> lid == lid'
  _ -> False

rememberIchtacasPrey :: (ReverseQueue m, HasCardDef card) => EnemyId -> card -> m ()
rememberIchtacasPrey eid (toCardDef -> card) = do
  scenarioI18n $ gameModifier
    ScenarioSource
    eid
    (UIModifier $ ImportantToScenario $ ikey "labels.ichtacasPrey")
  remember $ IchtacasPrey $ Labeled (toName card) eid `With` Envelope @"cardCode" card.cardCode

rememberIchtacasDestination :: (ReverseQueue m, HasCardDef card) => LocationId -> card -> m ()
rememberIchtacasDestination lid (toCardDef -> card) = do
  scenarioI18n $ gameModifier
    ScenarioSource
    lid
    (UIModifier $ ImportantToScenario $ ikey "labels.ichtacasDestination")
  remember $ IchtacasDestination $ Labeled (toName card) lid `With` Envelope @"cardCode" card.cardCode

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "threadsOfFate" a
