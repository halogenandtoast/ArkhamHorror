module Arkham.Location.CardDefs.TheScarletKeys.CongressOfTheKeys where

import Arkham.Location.CardDefs.Import

congressChamberLair :: CardDef
congressChamberLair =
  locationWithUnrevealed
    "09707"
    "Coterie Sanctuary"
    [Lair]
    Circle
    [Squiggle]
    "Congress Chamber"
    [Lair]
    Square
    [Squiggle]
    CongressOfTheKeys

congressChamberSanctum :: CardDef
congressChamberSanctum =
  locationWithUnrevealed
    "09706"
    "Coterie Sanctuary"
    [Sanctum]
    Circle
    [Squiggle]
    "Congress Chamber"
    [Sanctum]
    Square
    [Squiggle]
    CongressOfTheKeys

coterieLibraryLair :: CardDef
coterieLibraryLair =
  locationWithUnrevealed
    "09709"
    "Coterie Sanctuary"
    [Lair]
    Circle
    [Squiggle]
    "Coterie Library"
    [Lair]
    Triangle
    [Squiggle]
    CongressOfTheKeys

coterieLibrarySanctum :: CardDef
coterieLibrarySanctum =
  locationWithUnrevealed
    "09708"
    "Coterie Sanctuary"
    [Sanctum]
    Circle
    [Squiggle]
    "Coterie Library"
    [Sanctum]
    Triangle
    [Squiggle]
    CongressOfTheKeys

gravityDefyingClimb :: CardDef
gravityDefyingClimb =
  location
    "09713"
    "Gravity-Defying Climb"
    [Otherworld, Tower]
    Equals
    [Moon, Star]
    CongressOfTheKeys

scarletHallsLair :: CardDef
scarletHallsLair =
  location
    "09705"
    "Scarlet Halls"
    [Lair]
    Squiggle
    [Square, Triangle, Diamond, Circle]
    CongressOfTheKeys

scarletHallsSanctum :: CardDef
scarletHallsSanctum =
  location
    "09704"
    "Scarlet Halls"
    [Sanctum]
    Squiggle
    [Square, Triangle, Diamond, Circle]
    CongressOfTheKeys

theKeyReliquaryLair :: CardDef
theKeyReliquaryLair =
  locationWithUnrevealed
    "09711"
    "Coterie Sanctuary"
    [Lair]
    Circle
    [Squiggle]
    "The Key Reliquary"
    [Lair]
    Diamond
    [Squiggle]
    CongressOfTheKeys

theKeyReliquarySanctum :: CardDef
theKeyReliquarySanctum =
  locationWithUnrevealed
    "09710"
    "Coterie Sanctuary"
    [Sanctum]
    Circle
    [Squiggle]
    "The Key Reliquary"
    [Sanctum]
    Diamond
    [Squiggle]
    CongressOfTheKeys

theKnottedTower :: CardDef
theKnottedTower =
  location
    "09712"
    "The Knotted Tower"
    [Otherworld]
    Moon
    [Equals]
    CongressOfTheKeys

theToweringVertexRuinousConflux :: CardDef
theToweringVertexRuinousConflux =
  location
    "09714"
    ("The Towering Vertex" <:> "Ruinous Conflux")
    [Otherworld, Tower]
    Star
    [Equals]
    CongressOfTheKeys
