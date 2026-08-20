module Arkham.Location.CardDefs.ReturnToTheDunwichLegacy.ReturnToTheHouseAlwaysWins where

import Arkham.Location.CardDefs.Import

cloverClubStage :: CardDef
cloverClubStage =
  location
    "51017"
    "Clover Club Stage"
    [CloverClub]
    Heart
    [Circle, Square]
    ReturnToTheHouseAlwaysWins

exhibitHallMedievalExhibit :: CardDef
exhibitHallMedievalExhibit =
  victory 1
    $ locationWithUnrevealed
      "51021"
      "Exhibit Hall"
      [Miskatonic, Exhibit]
      NoSymbol
      [Square]
      ("Exhibit Hall" <:> "Medieval Exhibit")
      [Miskatonic, Exhibit]
      Star
      [Square]
      ReturnToTheHouseAlwaysWins

exhibitHallTheArchives :: CardDef
exhibitHallTheArchives =
  locationWithUnrevealed
    "51022"
    "Exhibit Hall"
    [Miskatonic, Exhibit]
    NoSymbol
    [Square]
    ("Exhibit Hall" <:> "The Archives")
    [Miskatonic, Exhibit]
    Hourglass
    [Square, Equals]
    ReturnToTheHouseAlwaysWins

returnToCloverClubLounge :: CardDef
returnToCloverClubLounge =
  locationWithUnrevealed
    "51016"
    "Clover Club Lounge"
    [CloverClub]
    Circle
    [Moon, Square, Triangle]
    "Clover Club Lounge"
    [CloverClub]
    Circle
    [Moon, Square, Triangle, Heart]
    ReturnToTheHouseAlwaysWins
