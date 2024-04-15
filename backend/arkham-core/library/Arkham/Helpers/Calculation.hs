module Arkham.Helpers.Calculation where

import Arkham.Calculation
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv (getCard, getDistance)
import Arkham.Helpers.Agenda
import Arkham.Helpers.Cost
import Arkham.Helpers.Doom
import Arkham.Helpers.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Log
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest.Target
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Target
import Arkham.Token

calculate :: (HasCallStack, HasGame m) => GameCalculation -> m Int
calculate = go
 where
  go = \case
    Fixed n -> pure n
    MaxCalculation n d -> min n <$> go d
    DividedByCalculation d n -> (`div` n) <$> go d
    SumCalculation ds -> sum <$> traverse go ds
    SubtractCalculation d1 d2 -> (-) <$> go d1 <*> go d2
    RecordedCount key -> getRecordCount key
    ScenarioCount key -> scenarioCount key
    CountActs m -> selectCount m
    CountAgendas m -> selectCount m
    CountAssets m -> selectCount m
    CountEnemies mtchr -> selectCount mtchr
    CountEvents mtchr -> selectCount mtchr
    CountInvestigators mtchr -> selectCount mtchr
    CountLocations mtchr -> selectCount mtchr
    CountSkills mtchr -> selectCount mtchr
    CountTreacheries mtchr -> selectCount mtchr
    CountChaosTokens mtchr -> selectCount mtchr
    CurrentAgendaStepCalculation fallback -> do
      mAgenda <- selectOne AnyAgenda
      maybe (go fallback) getAgendaStep mAgenda
    AssetFieldCalculation aid fld -> field fld aid
    InvestigatorFieldCalculation iid fld -> field fld iid
    InvestigatorHandLengthCalculation iid -> fieldMap InvestigatorHand length iid
    EnemyMaybeFieldCalculation eid fld -> fromJustNote "missing maybe field" <$> field fld eid
    VictoryDisplayCountCalculation mtchr -> selectCount $ VictoryDisplayCardMatch mtchr
    EnemyMaybeGameValueFieldCalculation eid fld -> maybe (error "missing maybe field") getGameValue =<< field fld eid
    EnemyFieldCalculation eid fld -> field fld eid
    EnemyTargetFieldCalculation fld ->
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> field fld eid
        _ -> pure 0
    LocationFieldCalculation lid fld -> field fld lid
    InvestigatorLocationFieldCalculation iid fld -> do
      maybe (pure 0) (field fld) =<< field InvestigatorLocation iid
    CardCostCalculation iid' cId -> getCard cId >>= getModifiedCardCost iid'
    ScenarioInDiscardCountCalculation mtchr -> length <$> findInDiscard mtchr
    InvestigatorTokenCountCalculation iid token -> fieldMap InvestigatorTokens (countTokens token) iid
    DoomCountCalculation -> getDoomCount
    DistanceFromCalculation iid matcher -> do
      l1 <- getMaybeLocation iid
      l2 <- selectOne matcher
      case (l1, l2) of
        (Just l1', Just l2') -> maybe 0 unDistance <$> getDistance l1' l2'
        _ -> pure 0
    MaxAlarmLevelCalculation -> do
      -- getMaxAlarmLevel
      investigators <- select UneliminatedInvestigator
      alarmLevels <- traverse (fieldMap InvestigatorTokens (countTokens AlarmLevel)) investigators
      pure $ getMax0 $ foldMap Max0 alarmLevels
    DifferentClassAmong matcher -> do
      cards <- select matcher
      pure
        $ length
        $ filter (`notElem` [Neutral, Mythos])
        . nub
        $ concatMap (toList . cdClassSymbols . toCardDef . toCard) cards
    VengeanceCalculation -> do
      -- getVengeanceInVictoryDisplay
      victoryDisplay <- field ScenarioVictoryDisplay =<< selectJust TheScenario
      let
        isVengeanceCard = \case
          VengeanceCard _ -> True
          _ -> False
        inVictoryDisplay =
          sum $ map (fromMaybe 0 . cdVengeancePoints . toCardDef) victoryDisplay
        vengeanceCards = count isVengeanceCard victoryDisplay
      locationsWithModifier <-
        getSum
          <$> selectAgg
            (Sum . fromMaybe 0)
            LocationVengeance
            (LocationWithModifier InVictoryDisplayForCountingVengeance)
      pure $ inVictoryDisplay + locationsWithModifier + vengeanceCards
