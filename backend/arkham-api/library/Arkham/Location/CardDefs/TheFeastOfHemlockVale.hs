module Arkham.Location.CardDefs.TheFeastOfHemlockVale where

import Arkham.Location.CardDefs.Import

railIcons :: [GridDirection] -> CardDef -> CardDef
railIcons dirs def =
  def
    { cdMeta = mapFromList [("rails", toJSON dirs)]
    }

controlStation :: CardDef
controlStation =
  location_ "10508" "Control Station" [Rail, Station] WrittenInRock
    & railIcons [North, East]

railExit :: CardDef
railExit =
  victory 1
    $ location_ "10509" "Rail Exit" [Rail] WrittenInRock
    & railIcons [South]

leftTurnA :: CardDef
leftTurnA =
  locationWithUnrevealed_ "10510a" "Rail Tunnel" [Rail] "Left Turn" [Rail] WrittenInRock
    & railIcons [North, West]

leftTurnB :: CardDef
leftTurnB =
  locationWithUnrevealed_ "10510b" "Rail Tunnel" [Rail] "Left Turn" [Rail] WrittenInRock
    & railIcons [North, West]

rightTurnA :: CardDef
rightTurnA =
  locationWithUnrevealed_ "10511a" "Rail Tunnel" [Rail] "Right Turn" [Rail] WrittenInRock
    & railIcons [East, South]

rightTurnB :: CardDef
rightTurnB =
  locationWithUnrevealed_ "10511b" "Rail Tunnel" [Rail] "Right Turn" [Rail] WrittenInRock
    & railIcons [East, South]

alkalineRailA :: CardDef
alkalineRailA =
  locationWithUnrevealed_ "10512a" "Rail Tunnel" [Rail] "Alkaline Rail" [Rail] WrittenInRock
    & railIcons [East, West]

alkalineRailB :: CardDef
alkalineRailB =
  locationWithUnrevealed_ "10512b" "Rail Tunnel" [Rail] "Alkaline Rail" [Rail] WrittenInRock
    & railIcons [North, South]

warpedRailA :: CardDef
warpedRailA =
  victory 1
    $ locationWithUnrevealed_ "10513a" "Rail Tunnel" [Rail] "Warped Rail" [Rail] WrittenInRock
    & railIcons [East, West]

warpedRailB :: CardDef
warpedRailB =
  victory 1
    $ locationWithUnrevealed_ "10513b" "Rail Tunnel" [Rail] "Warped Rail" [Rail] WrittenInRock
    & railIcons [North, East, South]

sunkenRailA :: CardDef
sunkenRailA =
  locationWithUnrevealed_ "10514a" "Rail Tunnel" [Rail] "Sunken Rail" [Rail] WrittenInRock
    & railIcons [East, South, West]

sunkenRailB :: CardDef
sunkenRailB =
  locationWithUnrevealed_ "10514b" "Rail Tunnel" [Rail] "Sunken Rail" [Rail] WrittenInRock
    & railIcons [North, East, South]

forkedRail :: CardDef
forkedRail =
  quantity 3
    $ locationWithUnrevealed_ "10515" "Rail Tunnel" [Rail] "Forked Rail" [Rail] WrittenInRock
    & railIcons [North, East, South, West]

railBridge :: CardDef
railBridge =
  locationWithUnrevealed_ "10516" "Rail Tunnel" [Rail] "Rail Bridge" [Rail] WrittenInRock
    & railIcons [North, South]

bedroomHemlockHouse32 :: CardDef
bedroomHemlockHouse32 =
  otherSideIs "10532b"
    $ victory 0
    $ location_ "10532" "Bedroom" [Room, Dormant] HemlockHouse

bedroomHemlockHouse33 :: CardDef
bedroomHemlockHouse33 =
  otherSideIs "10533b"
    $ victory 0
    $ location_ "10533" "Bedroom" [Room, Dormant] HemlockHouse

bedroomHemlockHouse34 :: CardDef
bedroomHemlockHouse34 =
  otherSideIs "10534b"
    $ victory 0
    $ location_ "10534" "Bedroom" [Room, Dormant] HemlockHouse

bedroomHemlockHouse35 :: CardDef
bedroomHemlockHouse35 =
  otherSideIs "10535b"
    $ victory 0
    $ location_ "10535" "Bedroom" [Room, Dormant] HemlockHouse

washroomHemlockHouse36 :: CardDef
washroomHemlockHouse36 =
  otherSideIs "10536b"
    $ victory 0
    $ location_ "10536" "Washroom" [Room, Dormant] HemlockHouse

washroomHemlockHouse37 :: CardDef
washroomHemlockHouse37 =
  otherSideIs "10537b"
    $ victory 0
    $ location_ "10537" "Washroom" [Room, Dormant] HemlockHouse

washroomHemlockHouse38 :: CardDef
washroomHemlockHouse38 =
  otherSideIs "10538b"
    $ victory 0
    $ location_ "10538" "Washroom" [Room, Dormant] HemlockHouse

libraryHemlockHouse39 :: CardDef
libraryHemlockHouse39 =
  otherSideIs "10539b"
    $ victory 0
    $ location_ "10539" "Library" [Room, Dormant] HemlockHouse

libraryHemlockHouse40 :: CardDef
libraryHemlockHouse40 =
  otherSideIs "10540b"
    $ victory 0
    $ location_ "10540" "Library" [Room, Dormant] HemlockHouse

parlorHemlockHouse :: CardDef
parlorHemlockHouse =
  otherSideIs "10541b"
    $ victory 0
    $ location_ "10541" "Parlor" [Room, Dormant] HemlockHouse

diningRoomHemlockHouse :: CardDef
diningRoomHemlockHouse =
  otherSideIs "10542b"
    $ victory 0
    $ location_ "10542" "Dining Room" [Room, Dormant] HemlockHouse

foyerHemlockHouse :: CardDef
foyerHemlockHouse =
  otherSideIs "10543b"
    $ victory 0
    $ location_ "10543" "Foyer" [Room, Dormant] HemlockHouse

crystalGrove :: CardDef
crystalGrove = location "10555" "Crystal Grove" [Blight] Triangle [Equals, Circle] TheSilentHeath

pearlEstateRuins :: CardDef
pearlEstateRuins = location "10556" "Pearl Estate Ruins" [Ruins] Equals [Triangle, Circle] TheSilentHeath

ashenSlope :: CardDef
ashenSlope = location "10557" "Ashen Slope" [Blight] Circle [Triangle, Equals] TheSilentHeath

saltChamber :: CardDef
saltChamber = revelation $ singleSided $ location_ "10558" "Salt Chamber" [Cave, Lair] TheSilentHeath

larvalTunnel :: CardDef
larvalTunnel = revelation $ singleSided $ location_ "10559" "Larval Tunnel" [Cave, Lair] TheSilentHeath

crystalNursery :: CardDef
crystalNursery = revelation $ singleSided $ location_ "10560" "Crystal Nursery" [Cave, Lair, Blight] TheSilentHeath

glimmeringMeadow :: CardDef
glimmeringMeadow =
  locationWithUnrevealed_
    "10612"
    "Western Woods"
    [Forest, Dark]
    "Glimmering Meadow"
    [Forest, Dark]
    TheTwistedHollow

blightedGlade :: CardDef
blightedGlade =
  quantity 2
    $ locationWithUnrevealed_
      "10613"
      "Western Woods"
      [Forest, Dark]
      "Blighted Glade"
      [Forest, Blight, Dark]
      TheTwistedHollow

poisonedMarsh :: CardDef
poisonedMarsh =
  quantity 2
    $ locationWithUnrevealed_
      "10614"
      "Western Woods"
      [Forest, Dark]
      "Poisoned Marsh"
      [Forest, Blight, Dark]
      TheTwistedHollow

fecundThicket :: CardDef
fecundThicket =
  locationWithUnrevealed_
    "10615"
    "Western Woods"
    [Forest, Dark]
    "Fecund Thicket"
    [Forest, Dark]
    TheTwistedHollow

mushroomGrove :: CardDef
mushroomGrove =
  quantity 2
    $ locationWithUnrevealed_
      "10616"
      "Western Woods"
      [Forest, Dark]
      "Mushroom Grove"
      [Forest, Dark]
      TheTwistedHollow

moonlitClearing :: CardDef
moonlitClearing =
  locationWithUnrevealed_
    "10617"
    "Western Woods"
    [Forest, Dark]
    "Moonlit Clearing"
    [Forest, Lair, Dark]
    TheTwistedHollow

crookedPath :: CardDef
crookedPath =
  quantity 3
    $ locationWithUnrevealed_
      "10618"
      "Western Woods"
      [Forest, Dark]
      "Crooked Path"
      [Forest, Dark]
      TheTwistedHollow

corpseGrove :: CardDef
corpseGrove =
  locationWithUnrevealed_
    "10619"
    "Western Woods"
    [Forest, Dark]
    "Corpse Grove"
    [Forest, Lair, Dark]
    TheTwistedHollow

bearDen :: CardDef
bearDen =
  locationWithUnrevealed_
    "10620"
    "Western Woods"
    [Forest, Dark]
    "Bear Den"
    [Forest, Lair, Dark]
    TheTwistedHollow

theTwistedHollow :: CardDef
theTwistedHollow =
  victory 1
    $ locationWithUnrevealed_
      "10621"
      "Western Woods"
      [Forest, Dark]
      "The Twisted Hollow"
      [Forest, Dark]
      TheTwistedHollow

boardingHouseDay :: CardDef
boardingHouseDay =
  otherSideIs "10705b"
    $ location "10705a" "Boarding House" [HemlockVale] Circle [Diamond, Spade] TheVale

boardingHouseNight :: CardDef
boardingHouseNight =
  otherSideIs "10705a"
    $ location "10705b" "Boarding House" [HemlockVale] Circle [Diamond, Spade] TheVale

theCrossroadsDay :: CardDef
theCrossroadsDay =
  otherSideIs "10706b"
    $ location
      "10706a"
      "The Crossroads"
      [HemlockVale, Central]
      Diamond
      [Triangle, Square, Star, Circle, Heart, Moon]
      TheVale

theCrossroadsNight :: CardDef
theCrossroadsNight =
  otherSideIs "10706a"
    $ location
      "10706b"
      "The Crossroads"
      [HemlockVale, Central]
      Diamond
      [Triangle, Square, Star, Circle, Heart, Moon]
      TheVale

hemlockChapelDay :: CardDef
hemlockChapelDay =
  otherSideIs "10707b"
    $ location "10707a" "Hemlock Chapel" [HemlockVale] Triangle [Diamond, Moon] TheVale

hemlockChapelNight :: CardDef
hemlockChapelNight =
  otherSideIs "10707a"
    $ location "10707b" "Hemlock Chapel" [HemlockVale] Triangle [Diamond, Moon] TheVale

theOldMillDay :: CardDef
theOldMillDay =
  otherSideIs "10708b"
    $ location "10708a" "The Old Mill" [HemlockVale] Heart [Diamond, Moon] TheVale

theOldMillNight :: CardDef
theOldMillNight =
  otherSideIs "10708a"
    $ location "10708b" "The Old Mill" [HemlockVale] Heart [Diamond, Moon] TheVale

theAtwoodHouseDay :: CardDef
theAtwoodHouseDay =
  otherSideIs "10709b"
    $ location "10709a" "The Atwood House" [HemlockVale] Moon [Diamond, Triangle, Heart] TheVale

theAtwoodHouseNight :: CardDef
theAtwoodHouseNight =
  otherSideIs "10709a"
    $ location "10709b" "The Atwood House" [HemlockVale] Moon [Diamond, Triangle, Heart, Droplet] TheVale

tadsGeneralStoreDay :: CardDef
tadsGeneralStoreDay =
  otherSideIs "10710b"
    $ location "10710a" "Tad's General Store" [HemlockVale] Square [Diamond, Star] TheVale

tadsGeneralStoreNight :: CardDef
tadsGeneralStoreNight =
  otherSideIs "10710a"
    $ location "10710b" "Tad's General Store" [HemlockVale] Square [Diamond, Star] TheVale

valeSchoolhouseDay :: CardDef
valeSchoolhouseDay =
  otherSideIs "10711b"
    $ location "10711a" "Vale Schoolhouse" [HemlockVale] Spade [Star, Circle] TheVale

valeSchoolhouseNight :: CardDef
valeSchoolhouseNight =
  otherSideIs "10711a"
    $ location "10711b" "Vale Schoolhouse" [HemlockVale] Spade [Star, Circle] TheVale

theCommonsDay :: CardDef
theCommonsDay =
  otherSideIs "10712b"
    $ location "10712a" "The Commons" [HemlockVale] Star [Diamond, Square, Spade] TheVale

theCommonsNight :: CardDef
theCommonsNight =
  otherSideIs "10712a"
    $ location "10712b" "The Commons" [HemlockVale] Star [Diamond, Square, Spade] TheVale

dryBurrow :: CardDef
dryBurrow =
  quantity 2
    $ locationWithUnrevealed_ "10716" "Cavern" [Cave, Dark] "Dry Burrow" [Cave, Dark] HorrorsInTheRock

alkalineForest :: CardDef
alkalineForest =
  locationWithUnrevealed_
    "10717"
    "Cavern"
    [Cave, Dark]
    "Alkaline Forest"
    [Cave, Dark]
    HorrorsInTheRock

iridescentPassage :: CardDef
iridescentPassage =
  locationWithUnrevealed_
    "10718"
    "Cavern"
    [Cave, Dark]
    "Iridescent Passage"
    [Cave, Dark]
    HorrorsInTheRock

overgrownTunnel :: CardDef
overgrownTunnel =
  locationWithUnrevealed_
    "10719"
    "Cavern"
    [Cave, Dark]
    "Overgrown Tunnel"
    [Cave, Dark]
    HorrorsInTheRock

mineralTunnel :: CardDef
mineralTunnel =
  locationWithUnrevealed_ "10720" "Cavern" [Cave, Dark] "Mineral Tunnel" [Cave, Dark] HorrorsInTheRock
