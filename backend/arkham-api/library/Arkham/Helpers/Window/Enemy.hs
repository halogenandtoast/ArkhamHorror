{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 #-}

module Arkham.Helpers.Window.Enemy where

import {-# SOURCE #-} Arkham.Game ()
import Arkham.Attack.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Arkham.Window
import Arkham.Window qualified as Window
import Arkham.Zone

engagedEnemy :: HasCallStack => [Window] -> EnemyId
engagedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyEngaged _ eid) -> Just eid
    _ -> Nothing

evadingEnemy :: HasCallStack => [Window] -> EnemyId
evadingEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.AttemptToEvadeEnemy _ _ eid) -> Just eid
    _ -> Nothing

enteringEnemy :: HasCallStack => [Window] -> EnemyId
enteringEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyEnters eid _) -> Just eid
    (windowType -> Window.EnemyEntersYourLocation _ eid _) -> Just eid
    _ -> Nothing

defeatedEnemy :: HasCallStack => [Window] -> EnemyId
defeatedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyDefeated _ _ eid) -> Just eid
    (windowType -> Window.IfEnemyDefeated _ _ eid) -> Just eid
    (windowType -> Window.EnemyWouldBeDefeated eid) -> Just eid
    _ -> Nothing

attackedEnemy :: HasCallStack => [Window] -> EnemyId
attackedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.AttemptToFightEnemy _ _ eid) -> Just eid
    (windowType -> Window.EnemyAttacked _ _ eid) -> Just eid
    (windowType -> Window.SuccessfulAttackEnemy _ _ eid _) -> Just eid
    (windowType -> Window.FailAttackEnemy _ eid _) -> Just eid
    _ -> Nothing

attackingInvestigator :: HasCallStack => [Window] -> InvestigatorId
attackingInvestigator =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.AttemptToFightEnemy _ iid _) -> Just iid
    (windowType -> Window.EnemyAttacked iid _ _) -> Just iid
    (windowType -> Window.SuccessfulAttackEnemy iid _ _ _) -> Just iid
    (windowType -> Window.FailAttackEnemy iid _ _) -> Just iid
    _ -> Nothing

attackSource :: HasCallStack => [Window] -> Source
attackSource =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyAttacked _ source _) -> Just source
    _ -> Nothing

evadedEnemy :: HasCallStack => [Window] -> EnemyId
evadedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemyEvaded _ eid) -> Just eid
    (windowType -> Window.SuccessfulEvadeEnemy _ _ eid _) -> Just eid
    _ -> Nothing

spawnedEnemy :: HasCallStack => [Window] -> EnemyId
spawnedEnemy =
  fromMaybe (error "missing enemy") . asum . map \case
    (windowType -> Window.EnemySpawns eid _) -> Just eid
    (windowType -> Window.EnemyAttemptsToSpawnAt eid _) -> Just eid
    _ -> Nothing

getThatEnemy :: [Window] -> Maybe EnemyId
getThatEnemy = \case
  [] -> Nothing
  ((windowType -> Window.WouldReady (EnemyTarget eid)) : _) -> Just eid
  ((windowType -> Window.WouldPlaceDoom _ (EnemyTarget eid) _) : _) -> Just eid
  (_ : rest) -> getThatEnemy rest

getAttackDetails :: HasCallStack => [Window] -> EnemyAttackDetails
getAttackDetails = \case
  [] -> error "No attack details"
  ((windowType -> Window.EnemyWouldAttack details) : _) -> details
  ((windowType -> Window.EnemyAttacks details) : _) -> details
  ((windowType -> Window.EnemyAttacksEvenIfCancelled details) : _) -> details
  (_ : rest) -> getAttackDetails rest

getEnemy :: [Window] -> EnemyId
getEnemy = \case
  ((windowType -> Window.EnemySpawns eid _) : _) -> eid
  ((windowType -> Window.EnemyDefeated _ _ eid) : _) -> eid
  ((windowType -> Window.IfEnemyDefeated _ _ eid) : _) -> eid
  ((windowType -> Window.EnemyMoves eid _) : _) -> eid
  ((windowType -> Window.EnemyEnters eid _) : _) -> eid
  ((windowType -> Window.EnemyEntersYourLocation _ eid _) : _) -> eid
  ((windowType -> Window.EnemyWouldSpawnAt eid _) : _) -> eid
  ((windowType -> Window.EnemyAttacks details) : _) -> details.enemy
  ((windowType -> Window.WouldReady (EnemyTarget eid)) : _) -> eid
  ((windowType -> Window.WouldPlaceDoom _ (EnemyTarget eid) _) : _) -> eid
  ((windowType -> Window.PlacedDoom _ (EnemyTarget eid) _) : _) -> eid
  ((windowType -> Window.EnemyMovesTo _ _ eid) : _) -> eid
  ((windowType -> Window.EnemyWouldMove eid _ _ _) : _) -> eid
  ((windowType -> Window.WouldPatrol eid) : _) -> eid
  ((windowType -> Window.EnemyEngaged _ eid) : _) -> eid
  ((windowType -> Window.EnterPlay (EnemyTarget eid)) : _) -> eid
  (_ : rest) -> getEnemy rest
  _ -> error "invalid window"

getEnemies :: [Window] -> [EnemyId]
getEnemies = \case
  [] -> []
  ((windowType -> Window.EnemyEnters eid _) : rest) -> eid : getEnemies rest
  ((windowType -> Window.EnemyEntersYourLocation _ eid _) : rest) -> eid : getEnemies rest
  ((windowType -> Window.EnemyLeaves eid _) : rest) -> eid : getEnemies rest
  (_ : rest) -> getEnemies rest

damagedEnemy :: [Window] -> EnemyId
damagedEnemy = \case
  ((windowType -> Window.WouldTakeDamage _ (EnemyTarget eid) _ _) : _) -> eid
  ((windowType -> Window.DealtDamage _ _ (EnemyTarget eid) _) : _) -> eid
  _ -> error "Expected DealtDamage window"

damagedEnemyAmount :: [Window] -> Int
damagedEnemyAmount = \case
  ((windowType -> Window.WouldTakeDamage _ (EnemyTarget _) n _) : _) -> n
  ((windowType -> Window.DealtDamage _ _ (EnemyTarget _) n) : _) -> n
  _ -> error "Expected DealtDamage window"

enemyMatches :: (HasGame m, Tracing m) => EnemyId -> EnemyMatcher -> m Bool
enemyMatches _eid Matcher.AnyEnemy = pure True
enemyMatches eid matcher = orM [matches eid matcher, matches eid (Matcher.OutOfPlayEnemy RemovedZone matcher)]
