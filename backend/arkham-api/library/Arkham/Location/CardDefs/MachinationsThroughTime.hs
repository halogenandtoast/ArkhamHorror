module Arkham.Location.CardDefs.MachinationsThroughTime where

import Arkham.Location.CardDefs.Import

arkhamAdvertiserFuture :: CardDef
arkhamAdvertiserFuture =
  location "87025" "Arkham Advertiser" [Arkham, Portal, Future] Plus [Spade] MachinationsThroughTime

arkhamAdvertiserPresent :: CardDef
arkhamAdvertiserPresent =
  location
    "87016"
    "Arkham Advertiser"
    [Arkham, Portal, Present]
    Equals
    [Trefoil]
    MachinationsThroughTime

arkhamGazette :: CardDef
arkhamGazette =
  location "87007" "Arkham Gazette" [Arkham, Portal, Past] Star [Hourglass] MachinationsThroughTime

childhoodHome :: CardDef
childhoodHome =
  location "87011" "Childhood Home" [Arkham, Past] Heart [Triangle] MachinationsThroughTime

corriganIndustries :: CardDef
corriganIndustries =
  location "87029" "Corrigan Industries" [Arkham, Future] Squiggle [Diamond] MachinationsThroughTime

miskatonicUniversityFuture :: CardDef
miskatonicUniversityFuture =
  location
    "87028"
    "Miskatonic University"
    [Arkham, Portal, Future]
    Diamond
    [Spade, Squiggle]
    MachinationsThroughTime

miskatonicUniversityPast :: CardDef
miskatonicUniversityPast =
  location
    "87010"
    "Miskatonic University"
    [Arkham, Portal, Past]
    Triangle
    [Hourglass, Heart]
    MachinationsThroughTime

miskatonicUniversityPresent :: CardDef
miskatonicUniversityPresent =
  location
    "87019"
    "Miskatonic University"
    [Arkham, Portal, Present]
    Circle
    [Trefoil, Moon]
    MachinationsThroughTime

oMalleysWatchShop :: CardDef
oMalleysWatchShop =
  victory 1
    $ location
      "87008"
      "O'Malley's Watch Shop"
      [Arkham, Portal, Past]
      Hourglass
      [Star, Triangle]
      MachinationsThroughTime

riverDocksFuture :: CardDef
riverDocksFuture =
  location
    "87027"
    "River Docks"
    [Arkham, Portal, Future]
    Spade
    [Plus, Diamond]
    MachinationsThroughTime

riverDocksPast :: CardDef
riverDocksPast =
  location
    "87009"
    "River Docks"
    [Arkham, Portal, Past]
    Hourglass
    [Star, Triangle]
    MachinationsThroughTime

riverDocksPresent :: CardDef
riverDocksPresent =
  location
    "87018"
    "River Docks"
    [Arkham, Portal, Present]
    Trefoil
    [Equals, Circle]
    MachinationsThroughTime

tickTockClubFuture :: CardDef
tickTockClubFuture =
  victory 1
    $ location
      "87026"
      "Tick-Tock Club"
      [Arkham, Portal, Future]
      Spade
      [Plus, Diamond]
      MachinationsThroughTime

tickTockClubPresent :: CardDef
tickTockClubPresent =
  victory 1
    $ location
      "87017"
      "Tick-Tock Club"
      [Arkham, Portal, Present]
      Trefoil
      [Equals, Circle]
      MachinationsThroughTime

tindalos :: CardDef
tindalos =
  ( location
      "87005"
      ("Tindalos" <:> "Realm of Angular Time")
      [Past, Present, Future]
      NoSymbol
      []
      MachinationsThroughTimeSingleGroup
  )
    { cdArt = "87005"
    }

yeOldeMagickShoppe :: CardDef
yeOldeMagickShoppe =
  location "87020" "Ye Olde Magick Shoppe" [Arkham, Present] Moon [Circle] MachinationsThroughTime
