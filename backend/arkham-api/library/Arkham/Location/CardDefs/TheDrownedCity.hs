module Arkham.Location.CardDefs.TheDrownedCity where

import Arkham.Location.CardDefs.Import

-- One Last Job
tillinghastEsoterica :: CardDef
tillinghastEsoterica =
  location_ "11509" ("Tillinghast Esoterica" <:> "Assorted Curiosities") [] OneLastJob

-- Gang hideouts: the reverse sides of the two "Questioning the Gangs" act copies.
hibbsRoadhouse :: CardDef
hibbsRoadhouse =
  victory 1 $ location_ "11504b" "Hibb's Roadhouse" [Arkham, Front] OneLastJob

laBellaLunaTheDrownedCity :: CardDef
laBellaLunaTheDrownedCity =
  victory 1 $ location_ "11505b" "La Bella Luna" [Arkham, Front] OneLastJob

-- The Western Wall
treacherousPathSlickSteps :: CardDef
treacherousPathSlickSteps =
  location_ "11521" ("Treacherous Path" <:> "Slick Steps") [Rlyeh, Walkway] TheWesternWall

treacherousPathErodedShelf :: CardDef
treacherousPathErodedShelf =
  location_ "11522" ("Treacherous Path" <:> "Eroded Shelf") [Rlyeh, Walkway] TheWesternWall

treacherousPathPrecariousClimb :: CardDef
treacherousPathPrecariousClimb =
  location_ "11523" ("Treacherous Path" <:> "Precarious Climb") [Rlyeh, Walkway] TheWesternWall

treacherousPathDeadlyPass :: CardDef
treacherousPathDeadlyPass =
  location_ "11524" ("Treacherous Path" <:> "Deadly Pass") [Rlyeh, Walkway] TheWesternWall

treacherousPathShallowDen :: CardDef
treacherousPathShallowDen =
  location_ "11525" ("Treacherous Path" <:> "Shallow Den") [Rlyeh, Walkway] TheWesternWall

sunkenStairway :: CardDef
sunkenStairway =
  location_ "11526" "Sunken Stairway" [Rlyeh, Walkway] TheWesternWall

drownedShanty :: CardDef
drownedShanty =
  location_ "11527" "Drowned Shanty" [Rlyeh, Walkway] TheWesternWall

shatteredRuins :: CardDef
shatteredRuins =
  victory 1 $ location_ "11528" "Shattered Ruins" [Rlyeh, Walkway, Glyph] TheWesternWall

obsidianFoundations :: CardDef
obsidianFoundations =
  location_ "11529" "Obsidian Foundations" [Rlyeh] TheWesternWall

westernWall_11530 :: CardDef
westernWall_11530 =
  location_ "11530" "Western Wall" [Rlyeh] TheWesternWall

-- TODO: back side of double-sided card (11532b)
underseaVault :: CardDef
underseaVault =
  storyOnBack' "11532b"
    $ victory 1
    $ location_ "11532" "Undersea Vault" [Vault, Glyph] TheWesternWall

-- The Drowned Quarter
-- TODO: back side of double-sided card (11540b)
barrierCoreInactive :: CardDef
barrierCoreInactive =
  location_ "11540" ("Barrier Core" <:> "Inactive") [Seafloor, Central] TheDrownedQuarter

barrierCoreActive :: CardDef
barrierCoreActive =
  location_ "11540b" ("Barrier Core" <:> "Active") [Seafloor, Central] TheDrownedQuarter

abyssalTrench :: CardDef
abyssalTrench =
  location_ "11541" "Abyssal Trench" [Seafloor] TheDrownedQuarter

drownedAcropolisEphemeralRuins :: CardDef
drownedAcropolisEphemeralRuins =
  location_ "11542" ("Drowned Acropolis" <:> "Ephemeral Ruins") [Seafloor] TheDrownedQuarter

drownedAcropolisCollapsedRuins :: CardDef
drownedAcropolisCollapsedRuins =
  location_ "11543" ("Drowned Acropolis" <:> "Collapsed Ruins") [Seafloor] TheDrownedQuarter

blastedRuinsSunkenCircle :: CardDef
blastedRuinsSunkenCircle =
  location_ "11544" ("Blasted Ruins" <:> "Sunken Circle") [Seafloor] TheDrownedQuarter

blastedRuinsCrumblingEdifices :: CardDef
blastedRuinsCrumblingEdifices =
  location_ "11545" ("Blasted Ruins" <:> "Crumbling Edifices") [Seafloor] TheDrownedQuarter

coralReefStatuaryGarden :: CardDef
coralReefStatuaryGarden =
  victory 1 $ location_ "11546" ("Coral Reef" <:> "Statuary Garden") [Seafloor] TheDrownedQuarter

coralReefFeedingGrounds :: CardDef
coralReefFeedingGrounds =
  victory 1 $ location_ "11547" ("Coral Reef" <:> "Feeding Grounds") [Seafloor] TheDrownedQuarter

ancientGallery :: CardDef
ancientGallery =
  victory 1 $ location_ "11548" "Ancient Gallery" [Seafloor] TheDrownedQuarter

-- The Apiary
-- TODO: back side of double-sided card (11559b)
apiaryEntranceBeckoningLight :: CardDef
apiaryEntranceBeckoningLight =
  location_ "11559" ("Apiary Entrance" <:> "Beckoning Light") [Apiary, Central] TheApiary

apiaryEntranceDangerousExit :: CardDef
apiaryEntranceDangerousExit =
  location_ "11559b" ("Apiary Entrance" <:> "Dangerous Exit") [Apiary, Central] TheApiary

fleshyPathsEasternBurrows :: CardDef
fleshyPathsEasternBurrows =
  singleSided
    $ location_ "11560" ("Fleshy Paths" <:> "Eastern Burrows") [Apiary] TheApiary

fleshyPathsWesternBurrows :: CardDef
fleshyPathsWesternBurrows =
  singleSided
    $ victory 1
    $ location_ "11561" ("Fleshy Paths" <:> "Western Burrows") [Apiary, Glyph] TheApiary

growingFields :: CardDef
growingFields =
  singleSided
    $ location_ "11562" "Growing Fields" [Apiary] TheApiary

churningChasm :: CardDef
churningChasm =
  singleSided
    $ location_ "11563" "Churning Chasm" [Apiary] TheApiary

corruptedVault :: CardDef
corruptedVault =
  singleSided
    $ victory 1
    $ location_ "11564" "Corrupted Vault" [Apiary, Glyph] TheApiary

luminousTunnels :: CardDef
luminousTunnels =
  singleSided
    $ location_ "11565" "Luminous Tunnels" [Apiary, Enclave] TheApiary

spawningGrounds :: CardDef
spawningGrounds =
  singleSided
    $ location_ "11566" "Spawning Grounds" [Apiary, Enclave] TheApiary

lostCampsite :: CardDef
lostCampsite =
  singleSided
    $ victory 1
    $ location_ "11567" "Lost Campsite" [Apiary, Enclave, Sanctum] TheApiary

graspingCorridor :: CardDef
graspingCorridor =
  singleSided
    $ location_ "11569" "Grasping Corridor" [Apiary, Nest] TheApiary

starvingCorridor :: CardDef
starvingCorridor =
  singleSided
    $ location_ "11570" "Starving Corridor" [Apiary, Nest] TheApiary

acidicCoelom :: CardDef
acidicCoelom =
  singleSided
    $ victory 1
    $ location_ "11571" "Acidic Coelom" [Apiary, Nest, Sanctum] TheApiary

centralChamber :: CardDef
centralChamber =
  location_ "11572" "Central Chamber" [Apiary, Nest, Central] TheApiary

-- TODO: back side of double-sided card (11579b)
hiddenVault :: CardDef
hiddenVault =
  storyOnBack' "11579b"
    $ victory 1
    $ location_ "11579" "Hidden Vault" [Apiary, Glyph] TheApiary

-- The Grand Vault
theGreatStair :: CardDef
theGreatStair =
  location_ "11593" "The Great Stair" [Rlyeh] TheGrandVault

movingPlatformObservationStation :: CardDef
movingPlatformObservationStation =
  location_ "11594" ("Moving Platform" <:> "Observation Station") [Rlyeh] TheGrandVault

coreOfTheVaultHeartOfTheMachine :: CardDef
coreOfTheVaultHeartOfTheMachine =
  location_
    "11595"
    ("Core of the Vault" <:> "Heart of the Machine")
    [Rlyeh, Sanctum, Glyph]
    TheGrandVault

shroudedCistern :: CardDef
shroudedCistern =
  location_ "11596" "Shrouded Cistern" [Rlyeh, Vault] TheGrandVault

chamberOfRecordsArm :: CardDef
chamberOfRecordsArm =
  victory 1 $ location_ "11598" "Chamber of Records" [Rlyeh, Vault, Glyph] TheGrandVault

chamberOfRecordsEarth :: CardDef
chamberOfRecordsEarth =
  victory 1 $ location_ "11599" "Chamber of Records" [Rlyeh, Vault, Glyph] TheGrandVault

otherworldlyMechanismsObsidianBulwark :: CardDef
otherworldlyMechanismsObsidianBulwark =
  location_ "11600" ("Otherworldly Mechanisms" <:> "Obsidian Bulwark") [Rlyeh, Vault] TheGrandVault

otherworldlyMechanismsSluiceControl :: CardDef
otherworldlyMechanismsSluiceControl =
  location_ "11601" ("Otherworldly Mechanisms" <:> "Sluice Control") [Rlyeh, Vault] TheGrandVault

otherworldlyMechanismsGrimeCoveredGears :: CardDef
otherworldlyMechanismsGrimeCoveredGears =
  location_ "11602" ("Otherworldly Mechanisms" <:> "Grime-Covered Gears") [Rlyeh, Vault] TheGrandVault

otherworldlyMechanismsInscrutableApparatus :: CardDef
otherworldlyMechanismsInscrutableApparatus =
  location_
    "11603"
    ("Otherworldly Mechanisms" <:> "Inscrutable Apparatus")
    [Rlyeh, Vault]
    TheGrandVault

chamberOfTheTabletUnsealed :: CardDef
chamberOfTheTabletUnsealed =
  location_ "11604" ("Chamber of the Tablet" <:> "Unsealed") [Rlyeh] TheGrandVault

-- Court of the Ancients
westAntechamber :: CardDef
westAntechamber =
  location_ "11619" "West Antechamber" [Rlyeh] CourtOfTheAncients

eastAntechamber :: CardDef
eastAntechamber =
  location_ "11620" "East Antechamber" [Rlyeh] CourtOfTheAncients

twistingCatwalks :: CardDef
twistingCatwalks =
  location_ "11621" ("Twisting Catwalks" <:> "Western Rise") [Rlyeh] CourtOfTheAncients

-- TODO: back side of double-sided card (11622b)
greatLiftInactive :: CardDef
greatLiftInactive =
  location_ "11622" ("Great Lift" <:> "Inactive") [Rlyeh, Lift] CourtOfTheAncients

greatLiftActive :: CardDef
greatLiftActive =
  location_ "11622b" ("Great Lift" <:> "Active") [Rlyeh, Lift] CourtOfTheAncients

ancientAltar :: CardDef
ancientAltar =
  victory 1 $ location_ "11623" "Ancient Altar" [Rlyeh] CourtOfTheAncients

ringLibraryArchiveOfTheStars :: CardDef
ringLibraryArchiveOfTheStars =
  location_ "11624" ("Ring Library" <:> "Archive of the Stars") [Rlyeh, Passageway] CourtOfTheAncients

ringLibraryArchiveOfTheAncients :: CardDef
ringLibraryArchiveOfTheAncients =
  location_ "11625" ("Ring Library" <:> "Archive of the Ancients") [Rlyeh] CourtOfTheAncients

loftyWalkwayArchiveOfDreams :: CardDef
loftyWalkwayArchiveOfDreams =
  location_ "11626" ("Lofty Walkway" <:> "Archive of Dreams") [Rlyeh, Passageway] CourtOfTheAncients

loftyWalkwayArchiveOfConflict :: CardDef
loftyWalkwayArchiveOfConflict =
  location_ "11627" ("Lofty Walkway" <:> "Archive of Conflict") [Rlyeh] CourtOfTheAncients

luminousArchivesArchiveOfHistory :: CardDef
luminousArchivesArchiveOfHistory =
  location_
    "11628"
    ("Luminous Archives" <:> "Archive of History")
    [Rlyeh, Passageway]
    CourtOfTheAncients

luminousArchivesArchiveOfMemory :: CardDef
luminousArchivesArchiveOfMemory =
  location_ "11629" ("Luminous Archives" <:> "Archive of Memory") [Rlyeh] CourtOfTheAncients

-- Obsidian Canyons
rlyehStreets :: CardDef
rlyehStreets =
  location_ "11648" "R'lyeh Streets" [Rlyeh, Central] ObsidianCanyons

centralSpire :: CardDef
centralSpire =
  location_ "11649" "Central Spire" [Rlyeh, Central] ObsidianCanyons

floatingSpire :: CardDef
floatingSpire =
  location_ "11650" "Floating Spire" [Rlyeh, Central] ObsidianCanyons

westernWall_11651 :: CardDef
westernWall_11651 =
  location_ "11651" "Western Wall" [Rlyeh, Central] ObsidianCanyons

ancientDome :: CardDef
ancientDome =
  location_ "11652" "Ancient Dome" [Rlyeh, Central] ObsidianCanyons

easternAthenaeum :: CardDef
easternAthenaeum =
  victory 1 $ location_ "11653" "Eastern Athenaeum" [Rlyeh, Summit, Glyph] ObsidianCanyons

westernAthenaeum :: CardDef
westernAthenaeum =
  victory 1 $ location_ "11654" "Western Athenaeum" [Rlyeh, Summit, Glyph] ObsidianCanyons

obsidianCliffs :: CardDef
obsidianCliffs =
  victory 1 $ location_ "11655" "Obsidian Cliffs" [Rlyeh, Summit] ObsidianCanyons

suspendedReef :: CardDef
suspendedReef =
  location_ "11656" "Suspended Reef" [Rlyeh, Summit] ObsidianCanyons

hangingShip :: CardDef
hangingShip =
  location_ "11657" "Hanging Ship" [Summit] ObsidianCanyons

ancientCanyons :: CardDef
ancientCanyons =
  location_ "11658" "Ancient Canyons" [Summit] ObsidianCanyons

dazzlingSkyline :: CardDef
dazzlingSkyline =
  location_ "11659" "Dazzling Skyline" [Summit] ObsidianCanyons

aerialWaterfall :: CardDef
aerialWaterfall =
  location_ "11660" "Aerial Waterfall" [Summit] ObsidianCanyons

magneticSpires :: CardDef
magneticSpires =
  location_ "11661" "Magnetic Spires" [Summit] ObsidianCanyons

-- TODO: back side of double-sided card (11662b)
glyphOrrery :: CardDef
glyphOrrery =
  storyOnBack' "11662b"
    $ victory 1
    $ location_ "11662" "Glyph Orrery" [Rlyeh, Summit, Glyph] ObsidianCanyons

-- Sepulchre of the Sleeper
dreamersRest :: CardDef
dreamersRest =
  location_ "11676" "Dreamer's Rest" [Rlyeh, Lair] SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfAmbition :: CardDef
sigilCarvedAlcoveStoryOfAmbition =
  location_
    "11677"
    ("Sigil-Carved Alcove" <:> "Story of Ambition")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfResilience :: CardDef
sigilCarvedAlcoveStoryOfResilience =
  location_
    "11678"
    ("Sigil-Carved Alcove" <:> "Story of Resilience")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfInfinity :: CardDef
sigilCarvedAlcoveStoryOfInfinity =
  location_
    "11679"
    ("Sigil-Carved Alcove" <:> "Story of Infinity")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfDefiance :: CardDef
sigilCarvedAlcoveStoryOfDefiance =
  location_
    "11680"
    ("Sigil-Carved Alcove" <:> "Story of Defiance")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

sigilCarvedAlcoveStoryOfTheVoyage :: CardDef
sigilCarvedAlcoveStoryOfTheVoyage =
  location_
    "11681"
    ("Sigil-Carved Alcove" <:> "Story of the Voyage")
    [Rlyeh, Glyph]
    SepulchreOfTheSleeper

-- The Doom of Arkham, Part I
tillinghastEsotericaEphemeralShop :: CardDef
tillinghastEsotericaEphemeralShop =
  singleSided
    $ victory 1
    $ location_
      "11685"
      ("Tillinghast Esoterica" <:> "Ephemeral Shop")
      [Sanctum, Extradimensional]
      TheDoomOfArkhamPartI

-- The Doom of Arkham, Part II
northsideTheDrownedCity :: CardDef
northsideTheDrownedCity =
  location_ "11692" ("Northside" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

downtown :: CardDef
downtown =
  location_ "11693" ("Downtown" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

easttownTheDrownedCity :: CardDef
easttownTheDrownedCity =
  location_ "11694" ("Easttown" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

miskatonicUniversityTheDrownedCity :: CardDef
miskatonicUniversityTheDrownedCity =
  location_ "11695" ("Miskatonic University" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

rivertownTheDrownedCity :: CardDef
rivertownTheDrownedCity =
  location_ "11696" ("Rivertown" <:> "Ruined") [Arkham, Central, Ruined] TheDoomOfArkhamPartII

stMarysHospitalTheDrownedCity :: CardDef
stMarysHospitalTheDrownedCity =
  location_ "11697" ("St. Mary's Hospital" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

southside :: CardDef
southside =
  location_ "11698" ("Southside" <:> "Ruined") [Arkham, Ruined] TheDoomOfArkhamPartII

westernRooftops :: CardDef
westernRooftops =
  location_ "11699" "Western Rooftops" [Arkham, Rooftop] TheDoomOfArkhamPartII

easternRooftops :: CardDef
easternRooftops =
  location_ "11700" "Eastern Rooftops" [Arkham, Rooftop] TheDoomOfArkhamPartII
