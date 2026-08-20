module Arkham.Location.CardDefs.TheDrownedCity.TheGrandVault where

import Arkham.Location.CardDefs.Import

chamberOfRecordsArm :: CardDef
chamberOfRecordsArm =
  victory 1 $ vaultChamber "11598" "Chamber of Records" [Rlyeh, Vault, Glyph]

chamberOfRecordsEarth :: CardDef
chamberOfRecordsEarth =
  victory 1 $ vaultChamber "11599" "Chamber of Records" [Rlyeh, Vault, Glyph]

chamberOfTheTabletUnsealed :: CardDef
chamberOfTheTabletUnsealed =
  locationWithUnrevealed_
    "11604"
    "Sealed Chamber"
    [Rlyeh]
    ("Chamber of the Tablet" <:> "Unsealed")
    [Rlyeh]
    TheGrandVault

coreOfTheVaultHeartOfTheMachine :: CardDef
coreOfTheVaultHeartOfTheMachine =
  location_
    "11595"
    ("Core of the Vault" <:> "Heart of the Machine")
    [Rlyeh, Sanctum, Glyph]
    TheGrandVault

movingPlatformObservationStation :: CardDef
movingPlatformObservationStation =
  location_ "11594" ("Moving Platform" <:> "Observation Station") [Rlyeh] TheGrandVault

otherworldlyMechanismsGrimeCoveredGears :: CardDef
otherworldlyMechanismsGrimeCoveredGears =
  vaultChamber "11602" ("Otherworldly Mechanisms" <:> "Grime-Covered Gears") [Rlyeh, Vault]

otherworldlyMechanismsInscrutableApparatus :: CardDef
otherworldlyMechanismsInscrutableApparatus =
  vaultChamber "11603" ("Otherworldly Mechanisms" <:> "Inscrutable Apparatus") [Rlyeh, Vault]

otherworldlyMechanismsObsidianBulwark :: CardDef
otherworldlyMechanismsObsidianBulwark =
  vaultChamber "11600" ("Otherworldly Mechanisms" <:> "Obsidian Bulwark") [Rlyeh, Vault]

otherworldlyMechanismsSluiceControl :: CardDef
otherworldlyMechanismsSluiceControl =
  vaultChamber "11601" ("Otherworldly Mechanisms" <:> "Sluice Control") [Rlyeh, Vault]

shroudedCistern :: CardDef
shroudedCistern =
  quantity 2 $ vaultChamber "11596" "Shrouded Cistern" [Rlyeh, Vault]

theGreatStair :: CardDef
theGreatStair =
  location_ "11593" "The Great Stair" [Rlyeh] TheGrandVault
