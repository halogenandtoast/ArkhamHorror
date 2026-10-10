module Arkham.Homebrew.AgesUnwound.CardDefs.Stories where

import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Story.CardDefs.Import

aidFromAfar :: CardDef
aidFromAfar = story ":ages-unwound:060" "Aid from Afar" Set.AWorldTornDown

destination :: CardDef
destination =
  otherSideIs ":ages-unwound:142"
    $ story ":ages-unwound:142b" "Destination" Set.Missions

bookHeist :: CardDef
bookHeist =
  otherSideIs ":ages-unwound:143"
    $ story ":ages-unwound:143b" "Book Heist" Set.Missions

aBlessingFromOnHigh :: CardDef
aBlessingFromOnHigh =
  otherSideIs ":ages-unwound:144"
    $ story ":ages-unwound:144b" "A Blessing from On High" Set.Missions

answers :: CardDef
answers =
  otherSideIs ":ages-unwound:145"
    $ story ":ages-unwound:145b" "Answers" Set.Missions

windowOfOpportunity :: CardDef
windowOfOpportunity =
  otherSideIs ":ages-unwound:146"
    $ story ":ages-unwound:146b" "Window of Opportunity" Set.Missions

gratitude :: CardDef
gratitude =
  otherSideIs ":ages-unwound:147"
    $ story ":ages-unwound:147b" "Gratitude" Set.Missions

favorsForFavors :: CardDef
favorsForFavors =
  otherSideIs ":ages-unwound:148"
    $ story ":ages-unwound:148b" "Favors for Favors" Set.Missions

grudgingAssistance :: CardDef
grudgingAssistance =
  otherSideIs ":ages-unwound:149"
    $ story ":ages-unwound:149b" "Grudging Assistance" Set.Missions

harnessingATearInReality :: CardDef
harnessingATearInReality =
  otherSideIs ":ages-unwound:152"
    $ story ":ages-unwound:152b" "Harnessing a Tear in Reality" Set.Missions

colourOutOfSpace :: CardDef
colourOutOfSpace =
  otherSideIs ":ages-unwound:154"
    $ story ":ages-unwound:154b" "Colour Out of Space" Set.Missions

aThousandPathsToVictory :: CardDef
aThousandPathsToVictory =
  otherSideIs ":ages-unwound:195"
    $ story ":ages-unwound:195b" "A Thousand Paths to Victory" Set.TimeRunsOut

aThousandAvenuesOfAttack :: CardDef
aThousandAvenuesOfAttack =
  otherSideIs ":ages-unwound:196"
    $ story ":ages-unwound:196b" "A Thousand Avenues of Attack" Set.TimeRunsOut

tideOfPossibility :: CardDef
tideOfPossibility =
  otherSideIs ":ages-unwound:197"
    $ story ":ages-unwound:197b" "Tide of Possibility" Set.TimeRunsOut

erasure :: CardDef
erasure =
  otherSideIs ":ages-unwound:199"
    $ story ":ages-unwound:199b" "Erasure" Set.TimeRunsOut

shatteringParadox :: CardDef
shatteringParadox =
  otherSideIs ":ages-unwound:200"
    $ story ":ages-unwound:200b" "Shattering Paradox" Set.TimeRunsOut

oblivionBeckons :: CardDef
oblivionBeckons =
  otherSideIs ":ages-unwound:203"
    $ story ":ages-unwound:203b" "Oblivion Beckons" Set.TimeRunsOut

backfire :: CardDef
backfire =
  otherSideIs ":ages-unwound:231b"
    $ story ":ages-unwound:231" "Backfire" Set.NightOfTheRitual
