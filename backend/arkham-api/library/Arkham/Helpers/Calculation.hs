module Arkham.Helpers.Calculation (module Arkham.Helpers.Calculation, module Arkham.Calculation) where

import Arkham.Asset.Types (Field (..))
import Arkham.Calculation
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv (getCard, getDistance)
import Arkham.Helpers.Agenda
import Arkham.Helpers.Card (getModifiedCardCost)
import Arkham.Helpers.Doom
import Arkham.Helpers.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Log
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest.Target
import Arkham.Helpers.Slot
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Target
import Arkham.Token

calculatePrinted :: HasGame m => Maybe GameCalculation -> m Int
calculatePrinted = \case
  Nothing -> pure 0
  Just calculation -> calculate calculation

calculate :: (HasCallStack, HasGame m) => GameCalculation -> m Int
calculate = go
 where
  go = \case
    Fixed n -> pure n
    Negated n -> negate . abs <$> go n
    MaxCalculation n d -> min <$> go n <*> go d
    DividedByCalculation d n -> (`div` n) <$> go d
    SumCalculation ds -> sum <$> traverse go ds
    SubtractCalculation d1 d2 -> (-) <$> go d1 <*> go d2
    MultiplyCalculation d1 d2 -> (*) <$> go d1 <*> go d2
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
    ActFieldCalculation aid fld -> field fld aid
    InvestigatorFieldCalculation iid fld -> field fld iid
    InvestigatorsFieldCalculation matcher fld -> getSum <$> selectAgg Sum fld matcher
    InvestigatorHandLengthCalculation iid -> fieldMap InvestigatorHand length iid
    InvestigatorKeyCountCalculation matcher -> length <$> selectAgg id InvestigatorKeys matcher
    LocationKeyCountCalculation matcher -> length <$> selectAgg id LocationKeys matcher
    EnemyMaybeFieldCalculation eid fld -> fromMaybe 0 . join <$> fieldMay fld eid
    SumEnemyMaybeFieldCalculation matcher fld -> do
      enemies <- select matcher
      getSum <$> foldMapM (fmap (Sum . fromMaybe 0 . join) . fieldMay fld) enemies
    VictoryDisplayCountCalculation mtchr -> selectCount $ VictoryDisplayCardMatch mtchr
    EnemyMaybeGameValueFieldCalculation eid fld -> maybe (error "missing maybe field") calculate =<< field fld eid
    EnemyFieldCalculation eid fld -> fromMaybe 0 <$> fieldMay fld eid
    EnemyTargetFieldCalculation fld ->
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> field fld eid
        _ -> pure 0
    LocationFieldCalculation lid fld -> field fld lid
    LocationGameValueFieldCalculation lid fld -> maybe (pure 0) getGameValue =<< fieldMay fld lid
    LocationMaybeFieldCalculation lid fld -> fromMaybe 0 . join <$> fieldMay fld lid
    -- In the boundary beyond if you pass a skill test it could trigger the act
    -- to advance, during that advancement it will cause the location to be
    -- removed mid-test so we need to need to just zero out the value. We may
    -- want to figure out how to memoize the calculation when the calculation
    -- fails.
    -- fromJustNote ("missing maybe field " <> show fld <> "<" <> show lid <> ">") <$> field fld lid
    InvestigatorLocationFieldCalculation iid fld -> do
      maybe (pure 0) (field fld) =<< field InvestigatorLocation iid
    InvestigatorLocationMaybeFieldCalculation iid fld -> do
      maybe (pure 0) (fieldMap fld (fromMaybe 0)) =<< field InvestigatorLocation iid
    CardCostCalculation iid' cId -> getCard cId >>= getModifiedCardCost iid'
    ScenarioInDiscardCountCalculation mtchr -> length <$> findInDiscard mtchr
    InvestigatorTokenCountCalculation iid token -> fieldMap InvestigatorTokens (countTokens token) iid
    AssetTokenCountCalculation aid token -> fieldMap AssetTokens (countTokens token) aid
    GameValueCalculation gv -> getGameValue gv
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
    DifferentClassAmong who matcher -> do
      iclasses <- selectField InvestigatorClass who
      cards <- select matcher
      pure
        $ length
        $ filter (`notElem` [Neutral, Mythos])
        . nub
        . (iclasses <>)
        $ concatMap (toList . cdClassSymbols . toCardDef . toCard) cards
    DuringEventCalculation c1 c2 -> do
      inEvent <- selectAny ActiveEvent
      go $ if inEvent then c1 else c2
    VengeanceCalculation -> do
      -- getVengeanceInVictoryDisplay
      victoryDisplay <- field ScenarioVictoryDisplay =<< selectJust TheScenario
      let
        isVengeanceCard = \case
          VengeanceCard _ -> True
          _ -> False
        inVictoryDisplay' =
          sum $ map (fromMaybe 0 . cdVengeancePoints . toCardDef) victoryDisplay
        vengeanceCards = count isVengeanceCard victoryDisplay
      locationsWithModifier <-
        getSum
          <$> selectAgg
            (Sum . fromMaybe 0)
            LocationVengeance
            (LocationWithModifier InVictoryDisplayForCountingVengeance)
      pure $ inVictoryDisplay' + locationsWithModifier + vengeanceCards
    EmptySlotsCalculation investigatorMatcher slotType -> do
      investigators <- select investigatorMatcher
      slots <- concatMapM (fieldMap InvestigatorSlots (findWithDefault [] slotType)) investigators
      pure $ count isEmptySlot slots
    AmountYouOweToBiancaDieKatz investigatorMatcher -> do
      selectOne investigatorMatcher >>= \case
        Nothing -> pure 0
        Just i -> do
          let
            toResources = \case
              (YouOweBiancaResources (Labeled _ iid') n) | i == iid' -> n
              _ -> 0
          sum . map toResources . toList <$> scenarioField ScenarioRemembered
    IfLocationExistsCalculation q c1 c2 -> do
      cond <- selectAny q
      calculate $ if cond then c1 else c2
    IfEnemyExistsCalculation q c1 c2 -> do
      cond <- selectAny q
      calculate $ if cond then c1 else c2
