module Arkham.Location.CardDefs.FilmFatale where

import Arkham.Location.CardDefs.Import

castleHallwaysSeeminglyEndless :: CardDef
castleHallwaysSeeminglyEndless =
  location
    "72050"
    ("Castle Hallways" <:> "Seemingly Endless")
    [Castle]
    Heart
    [Triangle, Square]
    AbominableContessa

catacombsStinksOfDeath :: CardDef
catacombsStinksOfDeath =
  location
    "72051"
    ("Catacombs" <:> "Stinks of Death")
    [Castle]
    Hourglass
    [Triangle, Trefoil]
    AbominableContessa

centralLotBlurred :: CardDef
centralLotBlurred =
  otherSideIs "72008"
    $ location
      "72008b"
      ("Central Lot" <:> "Blurred")
      [Set, Central, Extradimensional]
      Circle
      [Moon, Diamond, Triangle]
      FilmFatale

centralLotQuietOnSet :: CardDef
centralLotQuietOnSet =
  otherSideIs "72008b"
    $ location
      "72008"
      ("Central Lot" <:> "Quiet on Set")
      [Set, Central]
      Circle
      [Moon, Diamond, Triangle]
      FilmFatale

clockTowerIncessantlyTicking :: CardDef
clockTowerIncessantlyTicking =
  victory 1
    $ location
      "72052"
      ("Clock Tower" <:> "Incessantly Ticking")
      [Castle]
      Trefoil
      [Triangle, Hourglass]
      AbominableContessa

easternRidge :: CardDef
easternRidge = location "72041" "Eastern Ridge" [Jungle] Hourglass [T, Plus] ForgottenIsland

gothicSet :: CardDef
gothicSet =
  location
    "72011"
    "Gothic Set"
    [Set]
    Triangle
    [Circle, Heart, Hourglass, Trefoil, Square, Equals]
    FilmFatale

highRulersBastion :: CardDef
highRulersBastion = location "72027" "High Ruler's Bastion" [Cosmos] NoSymbol [] CosmicJourney

jungleRiver :: CardDef
jungleRiver = location "72042" "Jungle River" [Jungle] Plus [Droplet, Hourglass, Squiggle] ForgottenIsland

jungleSet :: CardDef
jungleSet = location "72010" "Jungle Set" [Set] Diamond [Circle, Droplet] FilmFatale

lostAsteroid :: CardDef
lostAsteroid =
  quantity 2
    $ singleSided
    $ revelation
    $ location "72034" "Lost Asteroid" [Cosmos] NoSymbol [] CosmicJourney

moonlitGardenPoisonedBeauty :: CardDef
moonlitGardenPoisonedBeauty =
  location
    "72053"
    ("Moonlit Garden" <:> "Poisoned Beauty")
    [Castle]
    Square
    [Triangle, Heart]
    AbominableContessa

ritualSiteTeetawn :: CardDef
ritualSiteTeetawn = location "72029" "Ritual Site Teetawn" [RitualSite] Squiggle [Droplet] CosmicJourney

ritualSiteTothis :: CardDef
ritualSiteTothis = location "72031" "Ritual Site Tothis" [RitualSite] Star [T] CosmicJourney

ruinsOfTheSerpentKing :: CardDef
ruinsOfTheSerpentKing =
  victory 1
    $ location "72043" "Ruins of the Serpent King" [Jungle, Ruins] Squiggle [Plus] ForgottenIsland

spaceSet :: CardDef
spaceSet = location "72009" "Space Set" [Set] Moon [Circle] FilmFatale

tarPit :: CardDef
tarPit = victory 1 $ location "72040" "Tar Pit" [Jungle] T [Droplet, Hourglass] ForgottenIsland

teetawnPassage :: CardDef
teetawnPassage = victory 1 $ location "72028" "Teetawn Passage" [Cosmos] Droplet [Squiggle] CosmicJourney

throneOfBloodRedAsBloodBlackAsNight :: CardDef
throneOfBloodRedAsBloodBlackAsNight =
  victory 1
    $ location
      "72054"
      ("Throne of Blood" <:> "Red as Blood, Black as Night")
      [Castle, Sanctum]
      Equals
      [Triangle]
      AbominableContessa

tothisBarrens :: CardDef
tothisBarrens = victory 1 $ location "72030" "Tothis Barrens" [Cosmos] T [Star] CosmicJourney

westernRidge :: CardDef
westernRidge = location "72039" "Western Ridge" [Jungle] Droplet [Diamond, T, Plus] ForgottenIsland
