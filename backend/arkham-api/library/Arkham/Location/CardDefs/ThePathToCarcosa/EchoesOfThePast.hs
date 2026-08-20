{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.ThePathToCarcosa.EchoesOfThePast where

import Arkham.Location.CardDefs.Import

entryHall :: CardDef
entryHall =
  location "03127" "Entry Hall" [GroundFloor] Square [Circle] EchoesOfThePast

hiddenLibrary :: CardDef
hiddenLibrary =
  victory 2
    $ location "03139" "Hidden Library" mempty NoSymbol [] EchoesOfThePast

historicalSocietyHistoricalLibrary_133 :: CardDef
historicalSocietyHistoricalLibrary_133 =
  locationWithUnrevealed
    "03133"
    "Historical Society"
    [SecondFloor]
    NoSymbol
    [Circle]
    ("Historical Society" <:> "Historical Library")
    [SecondFloor, Passageway]
    Triangle
    [Circle, Squiggle]
    EchoesOfThePast

historicalSocietyHistoricalLibrary_136 :: CardDef
historicalSocietyHistoricalLibrary_136 =
  locationWithUnrevealed
    "03136"
    "Historical Society"
    [ThirdFloor]
    NoSymbol
    [Star]
    ("Historical Society" <:> "Historical Library")
    [ThirdFloor, Passageway]
    Squiggle
    [Star, Triangle]
    EchoesOfThePast

historicalSocietyHistoricalMuseum_130 :: CardDef
historicalSocietyHistoricalMuseum_130 =
  locationWithUnrevealed
    "03130"
    "Historical Society"
    [GroundFloor]
    NoSymbol
    [Square]
    ("Historical Society" <:> "Historical Museum")
    [GroundFloor]
    Heart
    [Square, Hourglass]
    EchoesOfThePast

historicalSocietyHistoricalMuseum_132 :: CardDef
historicalSocietyHistoricalMuseum_132 =
  locationWithUnrevealed
    "03132"
    "Historical Society"
    [SecondFloor]
    NoSymbol
    [Circle]
    ("Historical Society" <:> "Historical Museum")
    [SecondFloor]
    Hourglass
    [Circle, Heart]
    EchoesOfThePast

historicalSocietyMeetingRoom :: CardDef
historicalSocietyMeetingRoom =
  locationWithUnrevealed
    "03128"
    "Historical Society"
    [GroundFloor]
    NoSymbol
    [Square]
    ("Historical Society" <:> "Meeting Room")
    [GroundFloor, Passageway]
    Diamond
    [Square]
    EchoesOfThePast

historicalSocietyPeabodysOffice :: CardDef
historicalSocietyPeabodysOffice =
  locationWithUnrevealed
    "03137"
    "Historical Society"
    [ThirdFloor]
    NoSymbol
    [Star]
    ("Historical Society" <:> "Peabody's Office")
    [ThirdFloor, Passageway]
    Moon
    [Star]
    EchoesOfThePast

historicalSocietyReadingRoom :: CardDef
historicalSocietyReadingRoom =
  locationWithUnrevealed
    "03134"
    "Historical Society"
    [SecondFloor]
    NoSymbol
    [Circle]
    ("Historical Society" <:> "Reading Room")
    [SecondFloor]
    T
    [Circle]
    EchoesOfThePast

historicalSocietyRecordOffice_129 :: CardDef
historicalSocietyRecordOffice_129 =
  locationWithUnrevealed
    "03129"
    "Historical Society"
    [GroundFloor]
    NoSymbol
    [Square]
    ("Historical Society" <:> "Record Office")
    [GroundFloor]
    Plus
    [Square]
    EchoesOfThePast

historicalSocietyRecordOffice_138 :: CardDef
historicalSocietyRecordOffice_138 =
  locationWithUnrevealed
    "03138"
    "Historical Society"
    [ThirdFloor]
    NoSymbol
    [Star]
    ("Historical Society" <:> "Record Office")
    [ThirdFloor]
    Equals
    [Star]
    EchoesOfThePast

quietHalls_131 :: CardDef
quietHalls_131 =
  location
    "03131"
    "Quiet Halls"
    [SecondFloor]
    Circle
    [Square, Star]
    EchoesOfThePast

quietHalls_135 :: CardDef
quietHalls_135 =
  location "03135" "Quiet Halls" [ThirdFloor] Star [Circle] EchoesOfThePast
