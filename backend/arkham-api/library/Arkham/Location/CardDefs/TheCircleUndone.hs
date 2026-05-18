{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheCircleUndone where

import Arkham.Location.CardDefs.Import
import Arkham.Trait qualified as Trait

witchesCircle :: CardDef
witchesCircle =
  revelation
    $ victory 2
    $ otherSideIs "05055"
    $ location "05055b" "Witches' Circle" [Woods, Trait.Circle] Plus [Squiggle] TheWitchingHour

witchHauntedWoodsAbandonedMine :: CardDef
witchHauntedWoodsAbandonedMine =
  victory 1
    $ locationWithUnrevealed
      "05058"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Abandoned Mine")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsCairnStones :: CardDef
witchHauntedWoodsCairnStones =
  victory 1
    $ locationWithUnrevealed
      "05059"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Cairn Stones")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsTheLonelyTree :: CardDef
witchHauntedWoodsTheLonelyTree =
  victory 1
    $ locationWithUnrevealed
      "05060"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "The Lonely Tree")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsChildsTreeHouse :: CardDef
witchHauntedWoodsChildsTreeHouse =
  victory 1
    $ locationWithUnrevealed
      "05061"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Child's Tree House")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsTaintedWell :: CardDef
witchHauntedWoodsTaintedWell =
  victory 1
    $ locationWithUnrevealed
      "05062"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Tainted Well")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsHermitsHouse :: CardDef
witchHauntedWoodsHermitsHouse =
  victory 1
    $ locationWithUnrevealed
      "05063"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Hermit's House")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

witchHauntedWoodsOvergrownBarn :: CardDef
witchHauntedWoodsOvergrownBarn =
  victory 1
    $ locationWithUnrevealed
      "05064"
      "Witch-Haunted Woods"
      [Woods]
      Squiggle
      [Squiggle, Plus]
      ("Witch-Haunted Woods" <:> "Overgrown Barn")
      [Woods]
      Squiggle
      [Squiggle, Plus]
      TheWitchingHour

entryHallAtDeathsDoorstep :: CardDef
entryHallAtDeathsDoorstep =
  location
    "05071"
    "Entry Hall"
    []
    Square
    [T]
    AtDeathsDoorstep

victorianHalls :: CardDef
victorianHalls =
  location
    "05072"
    "Victorian Halls"
    []
    T
    [Square, Star, Triangle, Heart]
    AtDeathsDoorstep

trophyRoom :: CardDef
trophyRoom =
  location
    "05073"
    "Trophy Room"
    []
    Triangle
    [T, Diamond]
    AtDeathsDoorstep

billiardsRoom :: CardDef
billiardsRoom =
  location
    "05074"
    "Billiards Room"
    []
    Diamond
    [Triangle]
    AtDeathsDoorstep

masterBedroom :: CardDef
masterBedroom =
  location
    "05075"
    "Master Bedroom"
    []
    Heart
    [T, Moon]
    AtDeathsDoorstep

balconyAtDeathsDoorstep :: CardDef
balconyAtDeathsDoorstep =
  location
    "05076"
    "Balcony"
    []
    Moon
    [Heart]
    AtDeathsDoorstep

office :: CardDef
office =
  location
    "05077"
    "Office"
    []
    Star
    [T]
    AtDeathsDoorstep

entryHallSpectral :: CardDef
entryHallSpectral =
  location
    "05078"
    "Entry Hall"
    [Spectral]
    Square
    [T]
    AtDeathsDoorstep

victorianHallsSpectral :: CardDef
victorianHallsSpectral =
  location
    "05079"
    "Victorian Halls"
    [Spectral]
    T
    [Square, Star, Triangle, Heart]
    AtDeathsDoorstep

trophyRoomSpectral :: CardDef
trophyRoomSpectral =
  location
    "05080"
    "Trophy Room"
    [Spectral]
    Triangle
    [T, Diamond]
    AtDeathsDoorstep

billiardsRoomSpectral :: CardDef
billiardsRoomSpectral =
  victory 1
    $ location
      "05081"
      "Billiards Room"
      [Spectral]
      Diamond
      [Triangle]
      AtDeathsDoorstep

masterBedroomSpectral :: CardDef
masterBedroomSpectral =
  victory 1
    $ location
      "05082"
      "Master Bedroom"
      [Spectral]
      Heart
      [T, Moon]
      AtDeathsDoorstep

balconySpectral :: CardDef
balconySpectral =
  location
    "05083"
    "Balcony"
    [Spectral]
    Moon
    [Heart]
    AtDeathsDoorstep

officeSpectral :: CardDef
officeSpectral =
  victory 2
    $ location
      "05084"
      "Office"
      [Spectral]
      Star
      [T]
      AtDeathsDoorstep

moldyHalls :: CardDef
moldyHalls =
  location
    "05128"
    "Moldy Halls"
    [WitchHouse]
    Triangle
    [Plus, Circle, Heart, Square, Diamond]
    TheSecretName

landlordsQuarters :: CardDef
landlordsQuarters =
  locationWithUnrevealed
    "05129"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Landlord's Quarters"
    [WitchHouse]
    Circle
    [Triangle]
    TheSecretName

joeMazurewiczsRoom :: CardDef
joeMazurewiczsRoom =
  locationWithUnrevealed
    "05130"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Joe Mazurewicz's Room"
    [WitchHouse]
    Heart
    [Triangle]
    TheSecretName

frankElwoodsRoom :: CardDef
frankElwoodsRoom =
  locationWithUnrevealed
    "05131"
    "Decrepit Door"
    [WitchHouse]
    Plus
    [Triangle]
    "Frank Elwood's room"
    [WitchHouse]
    Diamond
    [Triangle]
    TheSecretName

walterGilmansRoom :: CardDef
walterGilmansRoom =
  location
    "05132"
    "Walter Gilman's Room"
    [WitchHouse]
    Square
    [Triangle]
    TheSecretName

keziahsRoom :: CardDef
keziahsRoom =
  location
    "05133"
    "Keziah's Room"
    [Spectral, WitchHouse]
    Square
    [Moon, Hourglass, T, Equals, Squiggle]
    TheSecretName

moldyHallsEarlierTonight :: CardDef
moldyHallsEarlierTonight =
  locationWithUnrevealed
    "05134"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    ("Moldy Halls" <:> "Earlier Tonight")
    [Extradimensional, WitchHouse]
    Moon
    [Square]
    TheSecretName

twilightAbyss :: CardDef
twilightAbyss =
  victory 1
    $ locationWithUnrevealed
      "05135"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Twilight Abyss"
      [Extradimensional, Otherworld]
      Equals
      [Square, Squiggle]
      TheSecretName

cityOfElderThings :: CardDef
cityOfElderThings =
  victory 1
    $ locationWithUnrevealed
      "05136"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "City of Elder Things"
      [Extradimensional, Otherworld]
      Moon
      [Square]
      TheSecretName

witchHouseRuins :: CardDef
witchHouseRuins =
  locationWithUnrevealed
    "05137"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "Witch House Ruins"
    [Extradimensional, WitchHouse]
    Hourglass
    [Square, T]
    TheSecretName

salemGaol1692 :: CardDef
salemGaol1692 =
  locationWithUnrevealed
    "05138"
    "Unknown Places"
    [Extradimensional]
    Moon
    [Square]
    "Salem Gaol, 1692"
    [Extradimensional, Salem]
    Moon
    [Square]
    TheSecretName

physicsClassroom :: CardDef
physicsClassroom =
  victory 1
    $ locationWithUnrevealed
      "05139"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      "Physics Classroom"
      [Extradimensional, Miskatonic]
      Moon
      [Square]
      TheSecretName

courtOfTheGreatOldOnesANotTooDistantFuture :: CardDef
courtOfTheGreatOldOnesANotTooDistantFuture =
  victory 1
    $ locationWithUnrevealed
      "05140"
      "Unknown Places"
      [Extradimensional]
      Moon
      [Square]
      ("Court of the Great Old Ones" <:> "A Not-Too-Distant Future")
      [Extradimensional, Otherworld]
      Squiggle
      [Square, Equals]
      TheSecretName

siteOfTheSacrifice :: CardDef
siteOfTheSacrifice =
  location
    "05141"
    "Site of the Sacrifice"
    [Extradimensional, WitchHouse]
    T
    [Hourglass]
    TheSecretName

strangeGeometry :: CardDef
strangeGeometry =
  quantity 2
    $ singleSided
    $ location
      "05142"
      "Strange Geometry"
      [Extradimensional]
      NoSymbol
      []
      TheSecretName

hangmansBrook :: CardDef
hangmansBrook =
  otherSideIs "05166b"
    $ location "05166" "Hangman's Brook" mempty Squiggle [Circle, Plus] TheWagesOfSin

hangmansBrookSpectral :: CardDef
hangmansBrookSpectral =
  location "05166b" "Hangman's Brook" [Spectral] Squiggle [Circle, Plus] TheWagesOfSin
    & otherSideIs "05166"

hauntedFields :: CardDef
hauntedFields =
  victory 1
    $ otherSideIs "05167b"
    $ location "05167" "Haunted Fields" mempty Circle [Squiggle, Plus, Triangle, Square] TheWagesOfSin

hauntedFieldsSpectral :: CardDef
hauntedFieldsSpectral =
  victory 1
    $ otherSideIs "05167"
    $ location
      "05167b"
      "Haunted Fields"
      [Spectral]
      Circle
      [Squiggle, Plus, Triangle, Square]
      TheWagesOfSin

abandonedChapel :: CardDef
abandonedChapel =
  victory 1
    $ otherSideIs "05168b"
    $ location "05168" "Abandoned Chapel" mempty Plus [Squiggle, Circle, Diamond, Moon] TheWagesOfSin

abandonedChapelSpectral :: CardDef
abandonedChapelSpectral =
  victory 1
    $ otherSideIs "05168"
    $ location
      "05168b"
      "Abandoned Chapel"
      [Spectral]
      Plus
      [Squiggle, Circle, Diamond, Moon]
      TheWagesOfSin

theGallows_169 :: CardDef
theGallows_169 =
  otherSideIs "05169b"
    $ location "05169" "The Gallows" mempty Triangle [Circle, Square] TheWagesOfSin

theGallowsSpectral_169 :: CardDef
theGallowsSpectral_169 =
  otherSideIs "05169"
    $ location "05169b" "The Gallows" [Spectral] Triangle [Circle, Square] TheWagesOfSin

theGallows_170 :: CardDef
theGallows_170 =
  otherSideIs "05170b" $ location "05170" "The Gallows" mempty Triangle [Circle, Square] TheWagesOfSin

theGallowsSpectral_170 :: CardDef
theGallowsSpectral_170 =
  otherSideIs "05170"
    $ location
      "05170b"
      "The Gallows"
      [Spectral]
      Triangle
      [Circle, Square]
      TheWagesOfSin

hereticsGraves_171 :: CardDef
hereticsGraves_171 =
  otherSideIs "05171b"
    $ location "05171" "Heretics' Graves" mempty Square [Triangle, Circle] TheWagesOfSin

hereticsGravesSpectral_171 :: CardDef
hereticsGravesSpectral_171 =
  otherSideIs "05171"
    $ location
      "05171b"
      "Heretics' Graves"
      [Spectral]
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGraves_172 :: CardDef
hereticsGraves_172 =
  location "05172" "Heretics' Graves" mempty Square [Triangle, Circle] TheWagesOfSin
    & otherSideIs "05172b"

hereticsGravesSpectral_172 :: CardDef
hereticsGravesSpectral_172 =
  location "05172b" "Heretics' Graves" [Spectral] Square [Triangle, Circle] TheWagesOfSin
    & otherSideIs "05172"

chapelCrypt_173 :: CardDef
chapelCrypt_173 =
  location "05173" "Chapel Crypt" mempty Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05173b"

chapelCryptSpectral_173 :: CardDef
chapelCryptSpectral_173 =
  location "05173b" "Chapel Crypt" [Spectral] Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05173"

chapelCrypt_174 :: CardDef
chapelCrypt_174 =
  location "05174" "Chapel Crypt" mempty Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05174b"

chapelCryptSpectral_174 :: CardDef
chapelCryptSpectral_174 =
  location "05174b" "Chapel Crypt" [Spectral] Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05174"

chapelAttic_175 :: CardDef
chapelAttic_175 =
  location "05175" "Chapel Attic" mempty Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05175b"

chapelAtticSpectral_175 :: CardDef
chapelAtticSpectral_175 =
  location "05175b" "Chapel Attic" [Spectral] Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05175"

chapelAttic_176 :: CardDef
chapelAttic_176 =
  location "05176" "Chapel Attic" mempty Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05176b"

chapelAtticSpectral_176 :: CardDef
chapelAtticSpectral_176 =
  location "05176b" "Chapel Attic" [Spectral] Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05176"

lodgeGatesWeveBeenExpectingYou :: CardDef
lodgeGatesWeveBeenExpectingYou =
  location
    "05204"
    ("Lodge Gates" <:> "We've Been Expecting You")
    [Lodge]
    Diamond
    [Circle]
    ForTheGreaterGood

lodgeGatesMembersOnly :: CardDef
lodgeGatesMembersOnly =
  location
    "05205"
    ("Lodge Gates" <:> "Members Only")
    [Lodge]
    Diamond
    [T]
    ForTheGreaterGood

lobbyWeveBeenExpectingYou :: CardDef
lobbyWeveBeenExpectingYou =
  location
    "05206"
    ("Lobby" <:> "We've Been Expecting You")
    [Lodge]
    Circle
    [Diamond, T, Moon]
    ForTheGreaterGood

lobbyMembersOnly :: CardDef
lobbyMembersOnly =
  location
    "05207"
    ("Lobby" <:> "Members Only")
    [Lodge]
    Circle
    [T, Moon]
    ForTheGreaterGood

lodgeCellarWeveBeenExpectingYou :: CardDef
lodgeCellarWeveBeenExpectingYou =
  location
    "05208"
    ("Lodge Cellar" <:> "We've Been Expecting You")
    [Lodge]
    T
    [Circle, Squiggle]
    ForTheGreaterGood

lodgeCellarMembersOnly :: CardDef
lodgeCellarMembersOnly =
  location
    "05209"
    ("Lodge Cellar" <:> "Members Only")
    [Lodge]
    T
    [Diamond, Circle, Squiggle]
    ForTheGreaterGood

lounge :: CardDef
lounge =
  location
    "05210"
    "Lounge"
    [Lodge]
    Moon
    [Circle, Heart, Plus]
    ForTheGreaterGood

vault :: CardDef
vault =
  victory 1
    $ location
      "05211"
      "Vault"
      [Lodge]
      Plus
      [Moon]
      ForTheGreaterGood

library :: CardDef
library =
  victory 1
    $ location
      "05212"
      "Library"
      [Lodge]
      Heart
      [Moon]
      ForTheGreaterGood

lodgeCatacombs :: CardDef
lodgeCatacombs =
  location
    "05213"
    "Lodge Catacombs"
    [Lodge, Sanctum]
    Squiggle
    [T, Star, Triangle, Square, Hourglass]
    ForTheGreaterGood

sanctumDoorwayCeremonyRoom :: CardDef
sanctumDoorwayCeremonyRoom =
  victory 2
    $ locationWithUnrevealed
      "05214"
      "Sanctum Doorway"
      [Lodge, Sanctum]
      Star
      [Squiggle]
      "Ceremony Room"
      [Lodge, Sanctum]
      Triangle
      [Squiggle]
      ForTheGreaterGood

sanctumDoorwayHoldingCells :: CardDef
sanctumDoorwayHoldingCells =
  locationWithUnrevealed
    "05215"
    "Sanctum Doorway"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Holding Cells"
    [Lodge, Sanctum]
    Square
    [Squiggle]
    ForTheGreaterGood

innerSanctum :: CardDef
innerSanctum =
  locationWithUnrevealed
    "05216"
    "Inner Sanctum"
    [Lodge, Sanctum]
    Star
    [Squiggle]
    "Inner Sanctum"
    [Lodge, Sanctum]
    Hourglass
    [Squiggle]
    ForTheGreaterGood

miskatonicRiver :: CardDef
miskatonicRiver =
  location
    "05249"
    "Miskatonic River"
    [River]
    Triangle
    [Moon]
    UnionAndDisillusion

forbiddingShore :: CardDef
forbiddingShore =
  location
    "05250"
    "Forbidding Shore"
    [Woods]
    Moon
    [Triangle, Squiggle]
    UnionAndDisillusion

unvisitedIsleStandingStones :: CardDef
unvisitedIsleStandingStones =
  victory 1
    $ locationWithUnrevealed
      "05251"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Standing Stones")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleMistyClearing :: CardDef
unvisitedIsleMistyClearing =
  victory 1
    $ locationWithUnrevealed
      "05252"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Misty Clearing")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleForsakenWoods :: CardDef
unvisitedIsleForsakenWoods =
  victory 1
    $ locationWithUnrevealed
      "05253"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Forsaken Woods")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleMossCoveredSteps :: CardDef
unvisitedIsleMossCoveredSteps =
  victory 1
    $ locationWithUnrevealed
      "05254"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Moss-Covered Steps")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleHauntedSpring :: CardDef
unvisitedIsleHauntedSpring =
  victory 1
    $ locationWithUnrevealed
      "05255"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Haunted Spring")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

unvisitedIsleDecayedWillow :: CardDef
unvisitedIsleDecayedWillow =
  victory 1
    $ locationWithUnrevealed
      "05256"
      "Unvisited Isle"
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      ("Unvisited Isle" <:> "Decayed Willow")
      [Woods]
      Squiggle
      [Squiggle, Moon, Plus]
      UnionAndDisillusion

theGeistTrap :: CardDef
theGeistTrap =
  victory 1
    $ location
      "05257"
      "The Geist-Trap"
      [Woods, Spectral]
      Plus
      [Squiggle]
      UnionAndDisillusion

frenchHill_290 :: CardDef
frenchHill_290 =
  location
    "05290"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    InTheClutchesOfChaos

frenchHill_291 :: CardDef
frenchHill_291 =
  location
    "05291"
    "French Hill"
    [Arkham]
    T
    [Circle, Square, Star]
    InTheClutchesOfChaos

rivertown_292 :: CardDef
rivertown_292 =
  location
    "05292"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    InTheClutchesOfChaos

rivertown_293 :: CardDef
rivertown_293 =
  location
    "05293"
    "Rivertown"
    [Arkham]
    Circle
    [Square, Triangle, T]
    InTheClutchesOfChaos

southside_294 :: CardDef
southside_294 =
  location
    "05294"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    InTheClutchesOfChaos

southside_295 :: CardDef
southside_295 =
  location
    "05295"
    "Southside"
    [Arkham, Central]
    Square
    [Circle, Triangle, Plus, T, Diamond]
    InTheClutchesOfChaos

uptown_296 :: CardDef
uptown_296 =
  location
    "05296"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    InTheClutchesOfChaos

uptown_297 :: CardDef
uptown_297 =
  location
    "05297"
    "Uptown"
    [Arkham]
    Plus
    [Square, Triangle, Moon]
    InTheClutchesOfChaos

southChurch_298 :: CardDef
southChurch_298 =
  location
    "05298"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    InTheClutchesOfChaos

southChurch_299 :: CardDef
southChurch_299 =
  location
    "05299"
    "South Church"
    [Arkham]
    Diamond
    [Square]
    InTheClutchesOfChaos

merchantDistrict_300 :: CardDef
merchantDistrict_300 =
  location
    "05300"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    InTheClutchesOfChaos

merchantDistrict_301 :: CardDef
merchantDistrict_301 =
  location
    "05301"
    "Merchant District"
    [Arkham]
    Triangle
    [Circle, Square, Plus]
    InTheClutchesOfChaos

hangmansHillWhereItAllEnds :: CardDef
hangmansHillWhereItAllEnds =
  location
    "05302"
    ("Hangman's Hill" <:> "Where It All Ends")
    [Arkham]
    Moon
    [Plus]
    MusicOfTheDamned

silverTwilightLodgeShroudedInMystery :: CardDef
silverTwilightLodgeShroudedInMystery =
  victory 1
    $ location
      "05303"
      ("Silver Twilight Lodge" <:> "Shrouded In Mystery")
      [Arkham]
      Star
      [T]
      MusicOfTheDamned

hangmansHillShroudedInMystery :: CardDef
hangmansHillShroudedInMystery =
  victory 1
    $ location
      "05304"
      ("Hangman's Hill" <:> "Shrouded In Mystery")
      [Arkham]
      Moon
      [Plus]
      SecretsOfTheUniverse

silverTwilightLodgeWhereItAllEnds :: CardDef
silverTwilightLodgeWhereItAllEnds =
  location
    "05305"
    ("Silver Twilight Lodge" <:> "Where It All Ends")
    [Arkham]
    Star
    [T]
    SecretsOfTheUniverse

cosmicIngress :: CardDef
cosmicIngress =
  location
    "05332"
    "Cosmic Ingress"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

hideousPalace :: CardDef
hideousPalace =
  locationWithUnrevealed
    "05333"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Hideous Palace"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

courtOfTheGreatOldOnes :: CardDef
courtOfTheGreatOldOnes =
  locationWithUnrevealed
    "05334"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Court of the Great Old Ones"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

theBlackThrone :: CardDef
theBlackThrone =
  locationWithUnrevealed
    "05335"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "The Black Throne"
    [Otherworld]
    NoSymbol
    []
    BeforeTheBlackThrone

dancersMist :: CardDef
dancersMist =
  quantity 3
    $ locationWithUnrevealed
      "05336"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Dancer's Mist"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

flightIntoOblivion :: CardDef
flightIntoOblivion =
  quantity 3
    $ locationWithUnrevealed
      "05337"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Flight into Oblivion"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

infinityOfDarkness :: CardDef
infinityOfDarkness =
  quantity 3
    $ locationWithUnrevealed
      "05338"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Infinity of Darkness"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone

cosmicGate :: CardDef
cosmicGate =
  locationWithUnrevealed
    "05339"
    "Cosmos"
    [Otherworld]
    NoSymbol
    []
    "Cosmic Gate"
    [Otherworld, Void]
    NoSymbol
    []
    BeforeTheBlackThrone

pathwayIntoVoid :: CardDef
pathwayIntoVoid =
  quantity 2
    $ locationWithUnrevealed
      "05340"
      "Cosmos"
      [Otherworld]
      NoSymbol
      []
      "Pathway into Void"
      [Otherworld, Void]
      NoSymbol
      []
      BeforeTheBlackThrone
