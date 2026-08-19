module Arkham.Agenda.CardDefs.ThePathToCarcosa.BlackStarsRise where

import Arkham.Agenda.CardDefs.Import

theTideRises :: CardDef
theTideRises = agenda "03275" "The Tide Rises" 1 BlackStarsRise

theRitualBegins :: CardDef
theRitualBegins = agenda "03278" "The Ritual Begins" 1 BlackStarsRise

letTheStormRageTheVortexAbove :: CardDef
letTheStormRageTheVortexAbove = (agenda "03276a" "Let The Storm Rage" 2 BlackStarsRise) {cdOtherSide = Just "03276ab"}

letTheStormRageTheFloodBelow :: CardDef
letTheStormRageTheFloodBelow = agenda "03276b" "Let The Storm Rage" 2 BlackStarsRise

theEntityAboveTheFloodBelow :: CardDef
theEntityAboveTheFloodBelow = (agenda "03279a" "The Entity Above" 2 BlackStarsRise) {cdOtherSide = Just "03279ab"}

theEntityAboveTheVortexAbove :: CardDef
theEntityAboveTheVortexAbove = agenda "03279b" "The Entity Above" 2 BlackStarsRise

theCityFloods :: CardDef
theCityFloods = agenda "03277" "The City Floods" 3 BlackStarsRise

swallowedSky :: CardDef
swallowedSky = agenda "03280" "Swallowed Sky" 3 BlackStarsRise
