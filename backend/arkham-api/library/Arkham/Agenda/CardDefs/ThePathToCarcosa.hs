module Arkham.Agenda.CardDefs.ThePathToCarcosa where

import Arkham.Agenda.CardDefs.Import

theThirdAct :: CardDef
theThirdAct = agenda "03044" "The Third Act" 1 CurtainCall

encore :: CardDef
encore = agenda "03045" "Encore" 2 CurtainCall

fashionablyLate :: CardDef
fashionablyLate = agenda "03062" "Fashionably Late" 1 TheLastKing

theTerrifyingTruth :: CardDef
theTerrifyingTruth = agenda "03063" "The Terrifying Truth" 2 TheLastKing

theTruthIsHidden :: CardDef
theTruthIsHidden = agenda "03121" "The Truth is Hidden" 1 EchoesOfThePast

ransackingTheManor :: CardDef
ransackingTheManor = agenda "03122" "Ransacking the Manor" 2 EchoesOfThePast

secretsBetterLeftHidden :: CardDef
secretsBetterLeftHidden =
  agenda "03123" "Secrets Better Left Hidden" 3 EchoesOfThePast

lockedInside :: CardDef
lockedInside = agenda "03160" "Locked Inside" 1 TheUnspeakableOath

torturousDescent :: CardDef
torturousDescent = agenda "03161" "Torturous Descent" 2 TheUnspeakableOath

hisDomain :: CardDef
hisDomain = agenda "03162" "His Domain" 3 TheUnspeakableOath

theFirstNight :: CardDef
theFirstNight = agenda "03201" "The First Night" 1 APhantomOfTruth

theSecondNight :: CardDef
theSecondNight = agenda "03202" "The Second Night" 2 APhantomOfTruth

theThirdNight :: CardDef
theThirdNight = agenda "03203" "The Third Night" 3 APhantomOfTruth

empireOfTheDead :: CardDef
empireOfTheDead = agenda "03241" "Empire of the Dead" 1 ThePallidMask

empireOfTheUndead :: CardDef
empireOfTheUndead = agenda "03242" "Empire of the Undead" 2 ThePallidMask

theTideRises :: CardDef
theTideRises = agenda "03275" "The Tide Rises" 1 BlackStarsRise

letTheStormRageTheVortexAbove :: CardDef
letTheStormRageTheVortexAbove = (agenda "03276a" "Let The Storm Rage" 2 BlackStarsRise) {cdOtherSide = Just "03276ab"}

letTheStormRageTheFloodBelow :: CardDef
letTheStormRageTheFloodBelow = agenda "03276b" "Let The Storm Rage" 2 BlackStarsRise

theCityFloods :: CardDef
theCityFloods = agenda "03277" "The City Floods" 3 BlackStarsRise

theRitualBeginsBlackStarsRise :: CardDef
theRitualBeginsBlackStarsRise = agenda "03278" "The Ritual Begins" 1 BlackStarsRise

theEntityAboveTheFloodBelow :: CardDef
theEntityAboveTheFloodBelow = (agenda "03279a" "The Entity Above" 2 BlackStarsRise) {cdOtherSide = Just "03279ab"}

theEntityAboveTheVortexAbove :: CardDef
theEntityAboveTheVortexAbove = agenda "03279b" "The Entity Above" 2 BlackStarsRise

swallowedSky :: CardDef
swallowedSky = agenda "03280" "Swallowed Sky" 3 BlackStarsRise

madnessCoils :: CardDef
madnessCoils = agenda "03317" "Madness Coils" 1 DimCarcosa

madnessDrowns :: CardDef
madnessDrowns = agenda "03318" "Madness Drowns" 2 DimCarcosa

madnessDies :: CardDef
madnessDies = agenda "03319" "Madness Dies" 3 DimCarcosa
