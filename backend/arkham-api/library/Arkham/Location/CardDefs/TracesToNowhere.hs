module Arkham.Location.CardDefs.TracesToNowhere where

import Arkham.Location.CardDefs.Import

arkhamAdvertiser :: CardDef
arkhamAdvertiser =
  storyOnBack
    $ location "73027" "Arkham Advertiser" [Arkham, CrimeScene] Heart [Moon] TracesToNowhere

bonePit :: CardDef
bonePit =
  victory 1
    $ location "73052" "Bone Pit" [Otherworld] Trefoil [Diamond] DimensionalMadness

duterteFuneralHomeAfterHours :: CardDef
duterteFuneralHomeAfterHours =
  storyOnBack
    $ location
      "73030"
      ("Duterte Funeral Home" <:> "After Hours")
      [Arkham, CrimeScene]
      Circle
      [Star, Square]
      TracesToNowhere

duterteFuneralHomeSomethingInTheBasement :: CardDef
duterteFuneralHomeSomethingInTheBasement =
  storyOnBack
    $ location
      "73031"
      ("Duterte Funeral Home" <:> "Something in the Basement")
      [Arkham, CrimeScene]
      Circle
      [Star, Square]
      TracesToNowhere

etherealPerch :: CardDef
etherealPerch =
  victory 1
    $ location "73051" "Ethereal Perch" [Otherworld] Diamond [Droplet, Trefoil] DimensionalMadness

frenchHill :: CardDef
frenchHill =
  location "73010" "French Hill" [Arkham] Moon [Hourglass, Square, Equals, Heart] TracesToNowhere

historicalSociety :: CardDef
historicalSociety =
  locationWithUnrevealed
    "73012"
    "Historical Society"
    [Arkham]
    Square
    [Moon, Star, Circle, Equals]
    "Historical Society"
    [Arkham, CrimeScene]
    Square
    [Moon, Star, Circle, Equals]
    TracesToNowhere

meigerEstateBeyondTheBlackDoor :: CardDef
meigerEstateBeyondTheBlackDoor =
  storyOnBack
    $ location
      "73033"
      ("Meiger Estate" <:> "Beyond the Black Door")
      [Arkham, CrimeScene]
      Equals
      [Moon, Square]
      TracesToNowhere

meigerEstateUnderNewOwnership :: CardDef
meigerEstateUnderNewOwnership =
  storyOnBack
    $ location
      "73032"
      ("Meiger Estate" <:> "Under New Ownership")
      [Arkham, CrimeScene]
      Equals
      [Moon, Square]
      TracesToNowhere

merchantDistrict :: CardDef
merchantDistrict =
  location
    "73009"
    "Merchant District"
    [Arkham]
    Triangle
    [Hourglass, Star, T, Plus]
    TracesToNowhere

southChurch :: CardDef
southChurch =
  locationWithUnrevealed
    "73011"
    "South Church"
    [Arkham]
    Star
    [Triangle, Square, Circle, T]
    "South Church"
    [Arkham, CrimeScene]
    Star
    [Triangle, Square, Circle, T]
    TracesToNowhere

steersOffice :: CardDef
steersOffice =
  location "73008" "Steer's Office" [Arkham] Hourglass [Triangle, Moon] TracesToNowhere

thePortal :: CardDef
thePortal =
  victory 1
    $ location "73050" "The Portal" [Otherworld] Droplet [Diamond] DimensionalMadness

tickTockClub :: CardDef
tickTockClub =
  storyOnBack
    $ location "73029" "Tick-Tock Club" [Arkham, CrimeScene] T [Star, Triangle] TracesToNowhere

trainStation :: CardDef
trainStation =
  locationWithUnrevealed
    "73028"
    "Train Station"
    [Arkham]
    Plus
    [Triangle]
    "Train Station"
    [Arkham, CrimeScene]
    Plus
    [Triangle]
    TracesToNowhere
