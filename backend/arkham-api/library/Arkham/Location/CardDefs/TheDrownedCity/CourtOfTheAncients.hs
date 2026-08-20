module Arkham.Location.CardDefs.TheDrownedCity.CourtOfTheAncients where

import Arkham.Location.CardDefs.Import

ancientAltar :: CardDef
ancientAltar =
  victory 1 $ location "11623" "Ancient Altar" [Rlyeh] Plus [Heart] CourtOfTheAncients

eastAntechamber :: CardDef
eastAntechamber =
  location "11620" "East Antechamber" [Rlyeh] Heart [Plus] CourtOfTheAncients

greatLiftActive :: CardDef
greatLiftActive =
  otherSideIs "11622"
    $ location_ "11622b" ("Great Lift" <:> "Active") [Rlyeh, Lift] CourtOfTheAncients

greatLiftInactive :: CardDef
greatLiftInactive =
  otherSideIs "11622b"
    $ location_ "11622" ("Great Lift" <:> "Inactive") [Rlyeh, Lift] CourtOfTheAncients

loftyWalkwayArchiveOfConflict :: CardDef
loftyWalkwayArchiveOfConflict =
  locationWithUnrevealed_
    "11627"
    crumblingArchives
    [Rlyeh]
    ("Lofty Walkway" <:> "Archive of Conflict")
    [Rlyeh]
    CourtOfTheAncients

loftyWalkwayArchiveOfDreams :: CardDef
loftyWalkwayArchiveOfDreams =
  locationWithUnrevealed_
    "11626"
    crumblingArchives
    [Rlyeh]
    ("Lofty Walkway" <:> "Archive of Dreams")
    [Rlyeh, Passageway]
    CourtOfTheAncients

luminousArchivesArchiveOfHistory :: CardDef
luminousArchivesArchiveOfHistory =
  locationWithUnrevealed_
    "11628"
    crumblingArchives
    [Rlyeh]
    ("Luminous Archives" <:> "Archive of History")
    [Rlyeh, Passageway]
    CourtOfTheAncients

luminousArchivesArchiveOfMemory :: CardDef
luminousArchivesArchiveOfMemory =
  locationWithUnrevealed_
    "11629"
    crumblingArchives
    [Rlyeh]
    ("Luminous Archives" <:> "Archive of Memory")
    [Rlyeh]
    CourtOfTheAncients

ringLibraryArchiveOfTheAncients :: CardDef
ringLibraryArchiveOfTheAncients =
  locationWithUnrevealed_
    "11625"
    crumblingArchives
    [Rlyeh]
    ("Ring Library" <:> "Archive of the Ancients")
    [Rlyeh]
    CourtOfTheAncients

ringLibraryArchiveOfTheStars :: CardDef
ringLibraryArchiveOfTheStars =
  locationWithUnrevealed_
    "11624"
    crumblingArchives
    [Rlyeh]
    ("Ring Library" <:> "Archive of the Stars")
    [Rlyeh, Passageway]
    CourtOfTheAncients

twistingCatwalks :: CardDef
twistingCatwalks =
  location_ "11621" ("Twisting Catwalks" <:> "Western Rise") [Rlyeh] CourtOfTheAncients

westAntechamber :: CardDef
westAntechamber =
  location_ "11619" "West Antechamber" [Rlyeh] CourtOfTheAncients
