{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.ThePathToCarcosa.TheUnspeakableOath where

import Arkham.Location.CardDefs.Import

asylumHallsEasternPatientWing_170 :: CardDef
asylumHallsEasternPatientWing_170 =
  location
    "03170"
    ("Asylum Halls" <:> "Eastern Patient Wing")
    [ArkhamAsylum]
    Hourglass
    [Circle, Heart, Squiggle]
    TheUnspeakableOath

asylumHallsEasternPatientWing_171 :: CardDef
asylumHallsEasternPatientWing_171 =
  location
    "03171"
    ("Asylum Halls" <:> "Eastern Patient Wing")
    [ArkhamAsylum]
    Hourglass
    [Circle, Heart, Squiggle]
    TheUnspeakableOath

asylumHallsWesternPatientWing_168 :: CardDef
asylumHallsWesternPatientWing_168 =
  location
    "03168"
    ("Asylum Halls" <:> "Western Patient Wing")
    [ArkhamAsylum]
    Circle
    [Hourglass, Triangle, Diamond]
    TheUnspeakableOath

asylumHallsWesternPatientWing_169 :: CardDef
asylumHallsWesternPatientWing_169 =
  location
    "03169"
    ("Asylum Halls" <:> "Western Patient Wing")
    [ArkhamAsylum]
    Circle
    [Hourglass, Triangle, Diamond]
    TheUnspeakableOath

basementHall :: CardDef
basementHall =
  victory 1
    $ location
      "03177"
      "Basement Hall"
      [ArkhamAsylum]
      Squiggle
      [Hourglass, Moon]
      TheUnspeakableOath

garden :: CardDef
garden =
  location "03176" "Garden" [ArkhamAsylum] Plus [Diamond] TheUnspeakableOath

infirmary :: CardDef
infirmary =
  victory 1
    $ location
      "03174"
      "Infirmary"
      [ArkhamAsylum]
      Heart
      [Hourglass]
      TheUnspeakableOath

kitchen :: CardDef
kitchen =
  location "03172" "Kitchen" [ArkhamAsylum] Square [Triangle] TheUnspeakableOath

messHall :: CardDef
messHall =
  victory 1
    $ location
      "03173"
      "Mess Hall"
      [ArkhamAsylum]
      Triangle
      [Circle, Square]
      TheUnspeakableOath

patientConfinementDanielsCell :: CardDef
patientConfinementDanielsCell =
  locationWithUnrevealed
    "03178"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Daniel's Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

patientConfinementDrearyCell :: CardDef
patientConfinementDrearyCell =
  locationWithUnrevealed
    "03180"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Dreary Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

patientConfinementFamiliarCell :: CardDef
patientConfinementFamiliarCell =
  locationWithUnrevealed
    "03181"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Familiar Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

patientConfinementOccupiedCell :: CardDef
patientConfinementOccupiedCell =
  locationWithUnrevealed
    "03179"
    "Patient Confinement"
    mempty
    Moon
    [Squiggle]
    ("Patient Confinement" <:> "Occupied Cell")
    mempty
    Moon
    [Squiggle]
    TheUnspeakableOath

yard :: CardDef
yard =
  location
    "03175"
    "Yard"
    [ArkhamAsylum]
    Diamond
    [Circle, Plus]
    TheUnspeakableOath
