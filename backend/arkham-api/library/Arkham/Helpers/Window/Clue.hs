{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Helpers.Window.Clue where

import Arkham.Id
import Arkham.Prelude
import Arkham.Target
import Arkham.Window
import Arkham.Window qualified as Window

discoveredClues :: HasCallStack => [Window] -> Int
discoveredClues =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ _ _ n) -> Just n
    (windowType -> Window.WouldDiscoverClues _ _ _ _ n) -> Just n
    _ -> Nothing

getDiscover :: HasCallStack => [Window] -> DiscoverId
getDiscover =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.WouldDiscoverClues _ _ did _ _) -> Just did
    _ -> Nothing

discoveredCluesAt :: HasCallStack => [Window] -> (LocationId, Int)
discoveredCluesAt =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ n) -> Just (lid, n)
    (windowType -> Window.WouldDiscoverClues _ lid _ _ n) -> Just (lid, n)
    _ -> Nothing

discoveredLocation :: HasCallStack => [Window] -> LocationId
discoveredLocation =
  fromMaybe (error "missing discovery") . asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ _) -> Just lid
    (windowType -> Window.DiscoveringLastClue _ lid) -> Just lid
    _ -> Nothing

locationLeavingPlay :: HasCallStack => [Window] -> LocationId
locationLeavingPlay =
  fromMaybe (error "missing locationLeavingPlay") . asum . map \case
    (windowType -> Window.LeavePlay (LocationTarget lid)) -> Just lid
    _ -> Nothing

maybeDiscoveredLocation :: [Window] -> Maybe LocationId
maybeDiscoveredLocation =
  asum . map \case
    (windowType -> Window.DiscoverClues _ lid _ _) -> Just lid
    _ -> Nothing

entering :: HasCallStack => [Window] -> LocationId
entering =
  fromMaybe (error "missing enter window") . asum . map \case
    (windowType -> Window.Entering _ lid) -> Just lid
    _ -> Nothing

getRevealedLocation :: [Window] -> LocationId
getRevealedLocation = \case
  [] -> error "No location revealed"
  ((windowType -> Window.RevealLocation _ lid) : _) -> lid
  ((windowType -> Window.RevealLocationForcedAbilities _ lid _) : _) -> lid
  (_ : rest) -> getRevealedLocation rest

getInvestigatedLocation :: HasCallStack => [Window] -> LocationId
getInvestigatedLocation = \case
  [] -> error "No fail or pass skill test"
  ((windowType -> Window.FailInvestigationSkillTest _ lid _) : _) -> lid
  ((windowType -> Window.PassInvestigationSkillTest _ lid _) : _) -> lid
  (_ : rest) -> getInvestigatedLocation rest

getLocation :: [Window] -> Maybe LocationId
getLocation = \case
  [] -> Nothing
  ((windowType -> Window.EnemyEnters _ lid) : _) -> Just lid
  ((windowType -> Window.EnemyEntersYourLocation _ _ lid) : _) -> Just lid
  ((windowType -> Window.EnemyLeaves _ lid) : _) -> Just lid
  (_ : rest) -> getLocation rest
