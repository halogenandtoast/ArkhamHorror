module Arkham.Location.CardDefs.TheLabyrinthsOfLunacy where

import Arkham.Location.CardDefs.Import

abandonedWarehouse :: CardDef
abandonedWarehouse =
  location
    "70032"
    "Abandoned Warehouse"
    [Distortion]
    T
    [Diamond, Moon, Star]
    TheLabyrinthsOfLunacy

chamberOfDecay :: CardDef
chamberOfDecay =
  location "70029" "Chamber of Decay" [Distortion] Moon [Equals, T] TheLabyrinthsOfLunacy

chamberOfHunger :: CardDef
chamberOfHunger =
  location "70028" "Chamber of Hunger" [Distortion] Star [Equals, T] TheLabyrinthsOfLunacy

chamberOfNight :: CardDef
chamberOfNight =
  location
    "70023"
    "Chamber of Night"
    [Prison]
    Square
    [Triangle, Equals]
    LabyrinthsOfLunacySingleGroup

chamberOfNightEpicMultiplayer :: CardDef
chamberOfNightEpicMultiplayer =
  location
    "70022"
    "Chamber of Night"
    [Prison]
    Square
    [Triangle, Equals]
    LabyrinthsOfLunacyEpicMultiplayer

chamberOfPoison :: CardDef
chamberOfPoison =
  location "70031" "Chamber of Poison" [] Squiggle [Diamond] TheLabyrinthsOfLunacy

chamberOfRain :: CardDef
chamberOfRain =
  location "70019" "Chamber of Rain" [Distortion] Hourglass [Heart] TheLabyrinthsOfLunacy

chamberOfRegret :: CardDef
chamberOfRegret =
  location "70024" "Chamber of Regret" [Distortion] Triangle [Square] TheLabyrinthsOfLunacy

chamberOfRot :: CardDef
chamberOfRot =
  location "70030" "Chamber of Rot" [Distortion] Diamond [Equals, Squiggle, T] TheLabyrinthsOfLunacy

chamberOfSecretsBloodyPrison :: CardDef
chamberOfSecretsBloodyPrison =
  location
    "70016"
    ("Chamber of Secrets" <:> "Bloody Prison")
    [Prison, Distortion]
    Circle
    [Equals]
    TheLabyrinthsOfLunacy

chamberOfSecretsEnshroudedPrison :: CardDef
chamberOfSecretsEnshroudedPrison =
  location
    "70018"
    ("Chamber of Secrets" <:> "Enshrouded Prison")
    [Prison, Distortion]
    Circle
    [Equals]
    TheLabyrinthsOfLunacy

chamberOfSecretsMysteriousPrison :: CardDef
chamberOfSecretsMysteriousPrison =
  location
    "70017"
    ("Chamber of Secrets" <:> "Mysterious Prison")
    [Prison, Distortion]
    Circle
    [Equals]
    TheLabyrinthsOfLunacy

chamberOfSorrows :: CardDef
chamberOfSorrows =
  location
    "70021"
    "Chamber of Sorrows"
    [Prison]
    Heart
    [Hourglass, Equals]
    LabyrinthsOfLunacySingleGroup

chamberOfSorrowsEpicMultiplayer :: CardDef
chamberOfSorrowsEpicMultiplayer =
  location
    "70020"
    "Chamber of Sorrows"
    [Prison]
    Heart
    [Hourglass, Equals]
    LabyrinthsOfLunacyEpicMultiplayer

labyrinthineHallsCorpseFilledPath :: CardDef
labyrinthineHallsCorpseFilledPath =
  locationWithUnrevealed
    "70026"
    "Labyrinthine Halls"
    []
    Equals
    [Circle, Square, Heart, Equals]
    ("Labyrinthine Halls" <:> "Corpse-filled Path")
    []
    Equals
    [Circle, Square, Heart, Equals, Star]
    TheLabyrinthsOfLunacy

labyrinthineHallsFoulSmellingPath :: CardDef
labyrinthineHallsFoulSmellingPath =
  locationWithUnrevealed
    "70025"
    "Labyrinthine Halls"
    []
    Equals
    [Circle, Square, Heart, Equals]
    ("Labyrinthine Halls" <:> "Foul-smelling Path")
    []
    Equals
    [Circle, Square, Heart, Equals, Diamond]
    TheLabyrinthsOfLunacy

labyrinthineHallsOvergrownPath :: CardDef
labyrinthineHallsOvergrownPath =
  locationWithUnrevealed
    "70027"
    "Labyrinthine Halls"
    []
    Equals
    [Circle, Square, Heart, Equals]
    ("Labyrinthine Halls" <:> "Overgrown Path")
    []
    Equals
    [Circle, Square, Heart, Equals, Moon]
    TheLabyrinthsOfLunacy
