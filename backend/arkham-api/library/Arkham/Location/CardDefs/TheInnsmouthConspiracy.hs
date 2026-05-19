{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheInnsmouthConspiracy where

import Arkham.Location.CardDefs.Import

unfamiliarChamber :: CardDef
unfamiliarChamber =
  location
    "07047"
    "Unfamiliar Chamber"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

boneRiddenPit :: CardDef
boneRiddenPit =
  locationWithUnrevealed
    "07048"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Bone-Ridden Pit"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

fishGraveyard :: CardDef
fishGraveyard =
  locationWithUnrevealed
    "07049"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Fish Graveyard"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

altarToDagon :: CardDef
altarToDagon =
  locationWithUnrevealed
    "07050"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Altar to Dagon"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

idolChamber :: CardDef
idolChamber =
  locationWithUnrevealed
    "07051"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Idol Chamber"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

sealedExit :: CardDef
sealedExit =
  locationWithUnrevealed
    "07052"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Sealed Exit"
    [Cave]
    NoSymbol
    []
    ThePitOfDespair

marshRefinery :: CardDef
marshRefinery =
  location
    "07063"
    "Marsh Refinery"
    [Innsmouth]
    Circle
    [Triangle, Hourglass, Moon, Square, Heart]
    TheVanishingOfElinaHarper

gilmanHouse :: CardDef
gilmanHouse =
  location
    "07064"
    "Gilman House"
    [Innsmouth]
    Diamond
    [Triangle, Droplet, Star, Squiggle, T]
    TheVanishingOfElinaHarper

innsmouthSquare :: CardDef
innsmouthSquare =
  location
    "07065"
    "Innsmouth Square"
    [Innsmouth, Central]
    Triangle
    [Circle, Diamond, Star, Hourglass, Square, T]
    TheVanishingOfElinaHarper

innsmouthHarbour :: CardDef
innsmouthHarbour =
  location
    "07066"
    "Innsmouth Harbour"
    [Innsmouth]
    Moon
    [Circle, Hourglass, Equals, Heart]
    TheVanishingOfElinaHarper

firstNationalGrocery :: CardDef
firstNationalGrocery =
  location
    "07067"
    "First National Grocery"
    [Innsmouth]
    Star
    [Triangle, Diamond, Droplet, Square, Plus]
    TheVanishingOfElinaHarper

fishStreetBridge :: CardDef
fishStreetBridge =
  location
    "07068"
    "Fish Street Bridge"
    [Innsmouth]
    Hourglass
    [Triangle, Circle, Moon, Equals, T]
    TheVanishingOfElinaHarper

theLittleBookshop :: CardDef
theLittleBookshop =
  location
    "07069"
    "The Little Bookshop"
    [Innsmouth]
    Droplet
    [Diamond, Star, Squiggle, Plus]
    TheVanishingOfElinaHarper

esotericOrderOfDagon :: CardDef
esotericOrderOfDagon =
  singleSided
    $ victory 1
    $ location
      "07070"
      "Esoteric Order of Dagon"
      [Innsmouth, Hideout]
      Plus
      [Droplet, Star]
      TheVanishingOfElinaHarper

sawboneAlley :: CardDef
sawboneAlley =
  singleSided
    $ victory 1
    $ location
      "07071"
      "Sawbone Alley"
      [Innsmouth, Hideout]
      Squiggle
      [Droplet, Diamond]
      TheVanishingOfElinaHarper

shorewardSlums :: CardDef
shorewardSlums =
  singleSided
    $ victory 1
    $ location
      "07072"
      "Shoreward Slums"
      [Innsmouth, Hideout]
      Equals
      [Hourglass, Moon]
      TheVanishingOfElinaHarper

theHouseOnWaterStreet :: CardDef
theHouseOnWaterStreet =
  singleSided
    $ victory 1
    $ location
      "07073"
      "The House on Water Street"
      [Innsmouth, Hideout]
      Heart
      [Circle, Moon]
      TheVanishingOfElinaHarper

innsmouthJail :: CardDef
innsmouthJail =
  singleSided
    $ victory 1
    $ location
      "07074"
      "Innsmouth Jail"
      [Innsmouth, Hideout]
      T
      [Diamond, Triangle, Hourglass]
      TheVanishingOfElinaHarper

newChurchGreen :: CardDef
newChurchGreen =
  singleSided
    $ victory 1
    $ location
      "07075"
      "New Church Green"
      [Innsmouth, Hideout]
      Square
      [Circle, Triangle, Star]
      TheVanishingOfElinaHarper

underwaterCavern :: CardDef
underwaterCavern =
  quantity 2
    $ locationWithUnrevealed
      "07102"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Underwater Cavern"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

tidalPool :: CardDef
tidalPool =
  quantity 2
    $ locationWithUnrevealed
      "07103"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Tidal Pool"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

undergroundRiver :: CardDef
undergroundRiver =
  victory 1
    $ quantity 2
    $ locationWithUnrevealed
      "07104"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Underground River"
      [Cave]
      NoSymbol
      []
      FloodedCaverns

esotericOrderOfDagonInTooDeep :: CardDef
esotericOrderOfDagonInTooDeep =
  location
    "07129"
    "Esoteric Order of Dagon"
    [Innsmouth, Midtown]
    NoSymbol
    []
    InTooDeep

sawboneAlleyInTooDeep :: CardDef
sawboneAlleyInTooDeep =
  victory 1
    $ location
      "07130"
      "Sawbone Alley"
      [Innsmouth]
      NoSymbol
      []
      InTooDeep

shorewardSlumsInTooDeep :: CardDef
shorewardSlumsInTooDeep =
  location
    "07131"
    "Shoreward Slums"
    [Innsmouth, Coastal, Midtown]
    NoSymbol
    []
    InTooDeep

theHouseOnWaterStreetInTooDeep :: CardDef
theHouseOnWaterStreetInTooDeep =
  location
    "07132"
    "The House on Water Street"
    [Innsmouth, Coastal]
    NoSymbol
    []
    InTooDeep

innsmouthJailInTooDeep :: CardDef
innsmouthJailInTooDeep =
  victory 1
    $ location
      "07133"
      "Innsmouth Jail"
      [Innsmouth, Midtown]
      NoSymbol
      []
      InTooDeep

newChurchGreenInTooDeep :: CardDef
newChurchGreenInTooDeep =
  location
    "07134"
    "New Church Green"
    [Innsmouth, Midtown]
    NoSymbol
    []
    InTooDeep

theLittleBookshopInTooDeep :: CardDef
theLittleBookshopInTooDeep =
  location
    "07135"
    "The Little Bookshop"
    [Innsmouth]
    NoSymbol
    []
    InTooDeep

fishStreetBridgeInTooDeep :: CardDef
fishStreetBridgeInTooDeep =
  victory 1
    $ location "07136" "Fish Street Bridge" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

firstNationalGroceryInTooDeep :: CardDef
firstNationalGroceryInTooDeep =
  victory 1 $ location "07137" "First National Grocery" [Innsmouth, Midtown] NoSymbol [] InTooDeep

innsmouthHarbourInTooDeep :: CardDef
innsmouthHarbourInTooDeep =
  location "07138" "Fish Street Bridge" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

innsmouthSquareInTooDeep :: CardDef
innsmouthSquareInTooDeep =
  victory 1 $ location "07139" "Innsmouth Square" [Innsmouth, Midtown] NoSymbol [] InTooDeep

gilmanHouseInTooDeep :: CardDef
gilmanHouseInTooDeep = location "07140" "Gilman House" [Innsmouth, Midtown] NoSymbol [] InTooDeep

marshRefineryInTooDeep :: CardDef
marshRefineryInTooDeep = location "07141" "Marsh Refinery" [Innsmouth, Coastal, Midtown] NoSymbol [] InTooDeep

railroadStation :: CardDef
railroadStation = location "07142" "Railroad Station" [Innsmouth] NoSymbol [] InTooDeep

desolateCoastline :: CardDef
desolateCoastline = location "07143" "Desolate Coastline" [Innsmouth, Coastal] NoSymbol [] InTooDeep

churningWaters :: CardDef
churningWaters =
  location
    "07168"
    "Churning Waters"
    [Ocean]
    Triangle
    [Circle, Square, Heart, Star, Diamond, Plus]
    DevilReef

lonelyIsle :: CardDef
lonelyIsle =
  locationWithUnrevealed
    "07169"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Lonely Isle"
    [Ocean, Island]
    Square
    [Triangle, Plus, Diamond]
    DevilReef

hiddenCove :: CardDef
hiddenCove =
  locationWithUnrevealed
    "07170"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Hidden Cove"
    [Ocean, Island]
    Heart
    [Triangle, Diamond, Star]
    DevilReef

wavewornIsland :: CardDef
wavewornIsland =
  locationWithUnrevealed
    "07171"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Waveworn Island"
    [Ocean, Island]
    Star
    [Triangle, Plus, Heart]
    DevilReef

saltMarshes :: CardDef
saltMarshes =
  locationWithUnrevealed
    "07172"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Salt Marshes"
    [Ocean, Island]
    Diamond
    [Triangle, Heart, Square]
    DevilReef

blackReef :: CardDef
blackReef =
  locationWithUnrevealed
    "07173"
    "Devil Reef"
    [Ocean, Island]
    Circle
    [Triangle]
    "Black Reef"
    [Ocean, Island]
    Plus
    [Triangle, Square, Star]
    DevilReef

bootleggersHideaway_174a :: CardDef
bootleggersHideaway_174a =
  victory 1
    $ locationWithUnrevealed_
      "07174a"
      "Tidal Tunnel"
      [Cave]
      "Bootlegger's Hideaway"
      [Cave]
      DevilReef

bootleggersHideaway_174b :: CardDef
bootleggersHideaway_174b =
  victory 1
    $ locationWithUnrevealed_
      "07174b"
      "Tidal Tunnel"
      [Cave]
      "Bootlegger's Hideaway"
      [Cave]
      DevilReef

deepOneGrotto_175a :: CardDef
deepOneGrotto_175a =
  locationWithUnrevealed_
    "07175a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Deep One Grotto"
    [Cave, Yhanthlei]
    DevilReef

deepOneGrotto_175b :: CardDef
deepOneGrotto_175b =
  locationWithUnrevealed_
    "07175b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Deep One Grotto"
    [Cave, Yhanthlei]
    DevilReef

cyclopeanRuins_176a :: CardDef
cyclopeanRuins_176a =
  locationWithUnrevealed_
    "07176a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Cyclopean Ruins"
    [Cave, Yhanthlei]
    DevilReef

cyclopeanRuins_176b :: CardDef
cyclopeanRuins_176b =
  locationWithUnrevealed_
    "07176b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Cyclopean Ruins"
    [Cave, Yhanthlei]
    DevilReef

templeOfTheUnion_177a :: CardDef
templeOfTheUnion_177a =
  locationWithUnrevealed_
    "07177a"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Temple of the Union"
    [Cave, Yhanthlei]
    DevilReef

templeOfTheUnion_177b :: CardDef
templeOfTheUnion_177b =
  locationWithUnrevealed_
    "07177b"
    "Unfathomable Depths"
    [Cave, Yhanthlei]
    "Temple of the Union"
    [Cave, Yhanthlei]
    DevilReef

oldInnsmouthRoad :: CardCode -> Name -> CardDef
oldInnsmouthRoad cardCode name =
  locationWithUnrevealed_
    cardCode
    "Old Innsmouth Road"
    [Road]
    name
    [Road]
    HorrorInHighGear

falconPointApproach :: CardDef
falconPointApproach = oldInnsmouthRoad "07203" "Falcon Point Approach"

{- HLINT disable "Use camelCase" -}

-- road 1
dimlyLitRoad_a :: CardDef
dimlyLitRoad_a = oldInnsmouthRoad "07204a" "Dimly Lit Road"

-- road 1
dimlyLitRoad_b :: CardDef
dimlyLitRoad_b = oldInnsmouthRoad "07204b" "Dimly Lit Road"

-- road 1
dimlyLitRoad_c :: CardDef
dimlyLitRoad_c = oldInnsmouthRoad "07204c" "Dimly Lit Road"

-- road 1
cliffsideRoad_a :: CardDef
cliffsideRoad_a = victory 1 $ oldInnsmouthRoad "07205a" "Cliffside Road"

-- road 1
cliffsideRoad_b :: CardDef
cliffsideRoad_b = victory 1 $ oldInnsmouthRoad "07205b" "Cliffside Road"

-- road 2
forkInTheRoad_a :: CardDef
forkInTheRoad_a = victory 1 $ oldInnsmouthRoad "07206a" "Fork in the Road"

-- road 2
forkInTheRoad_b :: CardDef
forkInTheRoad_b = victory 1 $ oldInnsmouthRoad "07206b" "Fork in the Road"

-- road 3
intersection_a :: CardDef
intersection_a = victory 1 $ oldInnsmouthRoad "07207a" "Intersection"

-- road 3
intersection_b :: CardDef
intersection_b = victory 1 $ oldInnsmouthRoad "07207b" "Intersection"

-- road 1
tightTurn_a :: CardDef
tightTurn_a = oldInnsmouthRoad "07208a" "Tight Turn"

-- road 1
tightTurn_b :: CardDef
tightTurn_b = oldInnsmouthRoad "07208b" "Tight Turn"

-- road 1
tightTurn_c :: CardDef
tightTurn_c = oldInnsmouthRoad "07208c" "Tight Turn"

-- road 1
desolateRoad_a :: CardDef
desolateRoad_a = victory 1 $ oldInnsmouthRoad "07209a" "Desolate Road"

-- road 1
desolateRoad_b :: CardDef
desolateRoad_b = victory 1 $ oldInnsmouthRoad "07209b" "Desolate Road"

{- HLINT enable "Use camelCase" -}

-- road 1
longWayAround :: CardDef
longWayAround = quantity 6 $ oldInnsmouthRoad "07210" "Long Way Around"

falconPointGatehouse :: CardDef
falconPointGatehouse =
  location
    "07239"
    "Falcon Point Gatehouse"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

lighthouseStairwell :: CardDef
lighthouseStairwell =
  location
    "07240"
    "Lighthouse Stairwell"
    [FalconPoint]
    Equals
    [Triangle, Squiggle]
    ALightInTheFog

lanternRoom :: CardDef
lanternRoom =
  victory 1
    $ location
      "07241"
      "Lantern Room"
      [FalconPoint]
      Triangle
      [Equals]
      ALightInTheFog

falconPointCliffside :: CardDef
falconPointCliffside =
  location
    "07242"
    "Falcon Point Cliffside"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

lighthouseKeepersCottage :: CardDef
lighthouseKeepersCottage =
  location
    "07243"
    "Lighthouse Keeper's Cottage"
    [FalconPoint]
    NoSymbol
    []
    ALightInTheFog

sunkenGrottoUpperDepths :: CardDef
sunkenGrottoUpperDepths =
  locationWithUnrevealed
    "07244"
    "Lighthouse Basement"
    [Cave]
    Squiggle
    [Equals, Square, T]
    ("Sunken Grotto" <:> "Upper Depths")
    [Cave]
    Squiggle
    [Equals, Square, T]
    ALightInTheFog

sunkenGrottoLowerDepths :: CardDef
sunkenGrottoLowerDepths =
  location
    "07245"
    ("Sunken Grotto" <:> "Lower Depths")
    [Cave]
    Square
    [Squiggle, Diamond, T]
    ALightInTheFog

sunkenGrottoFinalDepths :: CardDef
sunkenGrottoFinalDepths =
  location
    "07246"
    ("Sunken Grotto" <:> "Final Depths")
    [Cave]
    Diamond
    [Square, T]
    ALightInTheFog

shrineToHydra :: CardDef
shrineToHydra =
  victory 1
    $ locationWithUnrevealed
      "07247"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Shrine to Hydra"
      [Cave]
      NoSymbol
      []
      ALightInTheFog

deepOneNursery :: CardDef
deepOneNursery =
  locationWithUnrevealed
    "07248"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Deep One Nursery"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

theMoonRoom :: CardDef
theMoonRoom =
  locationWithUnrevealed
    "07249"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "The Moon Room"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

sunkenArchives :: CardDef
sunkenArchives =
  victory 1
    $ locationWithUnrevealed_
      "07250"
      "Tidal Tunnel"
      [Cave]
      "Sunken Archives"
      [Cave]
      ALightInTheFog

pumpRoom :: CardDef
pumpRoom =
  locationWithUnrevealed
    "07251"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Pump Room"
    [Cave]
    NoSymbol
    []
    ALightInTheFog

holdingCells :: CardDef
holdingCells =
  victory 1
    $ otherSideIs "07252"
    $ location
      "07252b"
      "Holding Cells"
      [Cave]
      T
      [Squiggle, Square, Diamond]
      ALightInTheFog

grandEntryway :: CardDef
grandEntryway =
  location
    "07283"
    "Grand Entryway"
    [GroundFloor]
    Triangle
    [Square, Star, Plus, Trefoil]
    TheLairOfDagon

hallOfBlood :: CardDef
hallOfBlood =
  locationWithUnrevealed
    "07284"
    "First Floor Hall"
    [GroundFloor]
    Trefoil
    [Triangle]
    "Hall of Blood"
    [GroundFloor]
    Square
    [Triangle]
    TheLairOfDagon

hallOfTheDeep :: CardDef
hallOfTheDeep =
  victory 1
    $ locationWithUnrevealed
      "07285"
      "First Floor Hall"
      [GroundFloor]
      Trefoil
      [Triangle]
      "Hall of the Deep"
      [GroundFloor]
      Plus
      [Triangle]
      TheLairOfDagon

foulCorridors :: CardDef
foulCorridors =
  locationWithUnrevealed
    "07286"
    "Foul Corridors"
    [SecondFloor]
    Star
    [Triangle, Heart, Moon, Equals, Hourglass]
    "Foul Corridors"
    [SecondFloor, Passageway]
    Star
    [Triangle, Heart, Moon, Equals, Hourglass]
    TheLairOfDagon

hallOfLoyalty :: CardDef
hallOfLoyalty =
  victory 1
    $ locationWithUnrevealed
      "07287"
      "Second Floor Hall"
      [SecondFloor]
      Hourglass
      [Star]
      "Hall of Loyalty"
      [SecondFloor]
      Equals
      [Star]
      TheLairOfDagon

hallOfRebirth :: CardDef
hallOfRebirth =
  locationWithUnrevealed
    "07288"
    "Second Floor Hall"
    [SecondFloor]
    Hourglass
    [Star]
    "Hall of Rebirth"
    [SecondFloor]
    Heart
    [Star]
    TheLairOfDagon

hallOfSilence :: CardDef
hallOfSilence =
  locationWithUnrevealed
    "07289"
    "Third Floor Hall"
    [ThirdFloor]
    Moon
    [Star]
    "Hall of Silence"
    [ThirdFloor]
    Moon
    [Star]
    TheLairOfDagon

doorwayToTheDepths :: CardDef
doorwayToTheDepths =
  locationWithUnrevealed
    "07290"
    "Tidal Tunnel"
    [Cave]
    NoSymbol
    []
    "Doorway to the Depths"
    [Cave]
    Circle
    [Diamond]
    TheLairOfDagon

lairOfDagon :: CardDef
lairOfDagon =
  victory 1
    $ location
      "07291"
      "Lair of Dagon"
      [Yhanthlei, Lair]
      Diamond
      [Circle]
      TheLairOfDagon

darkAbyss :: CardDef
darkAbyss =
  quantity 2
    $ locationWithUnrevealed
      "07319"
      "Tidal Tunnel"
      [Cave]
      NoSymbol
      []
      "Dark Abyss"
      [Cave]
      NoSymbol
      []
      IntoTheMaelstrom

gatewayToYhanthlei :: CardDef
gatewayToYhanthlei =
  location
    "07320"
    "Gateway to Y'ha-nthlei"
    [Yhanthlei, Otherworld]
    NoSymbol
    []
    IntoTheMaelstrom

sunkenHalls :: CardDef
sunkenHalls =
  quantity 2
    $ locationWithUnrevealed
      "07321"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Sunken Halls"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

vaultOfRiches :: CardDef
vaultOfRiches =
  quantity 2
    $ locationWithUnrevealed
      "07322"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Vault of Riches"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

underseaCorridors :: CardDef
underseaCorridors =
  quantity 3
    $ locationWithUnrevealed
      "07323"
      "Y'ha-nthlei"
      [Yhanthlei]
      NoSymbol
      []
      "Undersea Corridors"
      [Yhanthlei]
      NoSymbol
      []
      IntoTheMaelstrom

statuesInTheDeep :: CardDef
statuesInTheDeep =
  locationWithUnrevealed
    "07324"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Statues in the Deep"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

submergedTemple :: CardDef
submergedTemple =
  locationWithUnrevealed
    "07325"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Submerged Temple"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

syzygyChamber :: CardDef
syzygyChamber =
  locationWithUnrevealed
    "07326"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Syzygy Chamber"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

onyxGuardians :: CardDef
onyxGuardians =
  locationWithUnrevealed
    "07327"
    "Y'ha-nthlei Sanctum"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    "Onyx Guardians"
    [Yhanthlei, Sanctum]
    NoSymbol
    []
    IntoTheMaelstrom

lairOfDagonIntoTheMaelstrom :: CardDef
lairOfDagonIntoTheMaelstrom =
  location
    "07328"
    ("Lair of Dagon" <:> "Sanctuary of Father Dagon")
    [Yhanthlei, Lair]
    NoSymbol
    []
    IntoTheMaelstrom

lairOfHydra :: CardDef
lairOfHydra =
  location
    "07329"
    ("Lair of Hydra" <:> "High Temple of Mother Hydra")
    [Yhanthlei, Lair]
    NoSymbol
    []
    IntoTheMaelstrom
