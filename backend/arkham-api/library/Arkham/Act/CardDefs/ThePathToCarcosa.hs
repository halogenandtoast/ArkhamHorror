module Arkham.Act.CardDefs.ThePathToCarcosa where

import Arkham.Act.CardDefs.Import

awakening :: CardDef
awakening = act "03046" "Awakening" 1 CurtainCall

theStrangerACityAflame :: CardDef
theStrangerACityAflame = (act "03047a" "The Stranger" 2 CurtainCall) {cdOtherSide = Just "03047ab"}

theStrangerThePathIsMine :: CardDef
theStrangerThePathIsMine = act "03047b" "The Stranger" 2 CurtainCall

theStrangerTheShoresOfHali :: CardDef
theStrangerTheShoresOfHali = act "03047c" "The Stranger" 2 CurtainCall

curtainCall :: CardDef
curtainCall = act "03048" "Curtain Call" 3 CurtainCall

discoveringTheTruth :: CardDef
discoveringTheTruth = act "03064" "Discovering the Truth" 1 TheLastKing

raceForAnswers :: CardDef
raceForAnswers = act "03124" "Race for Answers" 1 EchoesOfThePast

mistakesOfThePast :: CardDef
mistakesOfThePast = act "03125" "Mistakes of the Past" 2 EchoesOfThePast

theOath :: CardDef
theOath = act "03126" "The Oath" 3 EchoesOfThePast

arkhamAsylum :: CardDef
arkhamAsylum = act "03163" "Arkham Asylum" 1 TheUnspeakableOath

theReallyBadOnesV1 :: CardDef
theReallyBadOnesV1 =
  act "03164" "\"The Really Bad Ones\" (v. I)" 2 TheUnspeakableOath

theReallyBadOnesV2 :: CardDef
theReallyBadOnesV2 =
  act "03165" "\"The Really Bad Ones\" (v. II)" 2 TheUnspeakableOath

planningTheEscape :: CardDef
planningTheEscape = act "03166" "Planning the Escape" 3 TheUnspeakableOath

noAsylum :: CardDef
noAsylum = act "03167" "No Asylum" 4 TheUnspeakableOath

theParisianConspiracyV1 :: CardDef
theParisianConspiracyV1 =
  act "03204" "The Parisian Conspiracy (v. I)" 1 APhantomOfTruth

theParisianConspiracyV2 :: CardDef
theParisianConspiracyV2 =
  act "03205" "The Parisian Conspiracy (v. II)" 1 APhantomOfTruth

pursuingShadows :: CardDef
pursuingShadows = act "03206" "Pursuing Shadows" 2 APhantomOfTruth

stalkedByShadows :: CardDef
stalkedByShadows = act "03207" "Stalked by Shadows" 2 APhantomOfTruth

throughTheCatacombs :: CardDef
throughTheCatacombs = act "03243" "Through the Catacombs" 1 ThePallidMask

thePathIsBarred :: CardDef
thePathIsBarred = act "03244" "The Path is Barred" 2 ThePallidMask

theWayOut :: CardDef
theWayOut = act "03245" "The Way Out" 3 ThePallidMask

leadingTheWay :: CardDef
leadingTheWay = act "03246" "Leading the Way" 3 ThePallidMask

openThePathBelow :: CardDef
openThePathBelow = act "03281" "Open The Path Below" 3 BlackStarsRise

openThePathAbove :: CardDef
openThePathAbove = act "03282" "Open The Path Above" 3 BlackStarsRise

inLostCarcosa :: CardDef
inLostCarcosa = act "03320" "In Lost Carcosa" 1 DimCarcosa

searchForTheStrangerV1 :: CardDef
searchForTheStrangerV1 =
  (act "03321a" "Search For the Stranger (v.I)" 2 DimCarcosa)
    { cdOtherSide = Just "03321b"
    }

searchForTheStrangerV2 :: CardDef
searchForTheStrangerV2 =
  (act "03322a" "Search For the Stranger (v.II)" 2 DimCarcosa)
    { cdOtherSide = Just "03322ab"
    }

searchForTheStrangerV3 :: CardDef
searchForTheStrangerV3 =
  (act "03323a" "Search For the Stranger (v.III)" 2 DimCarcosa)
    { cdOtherSide = Just "03323ab"
    }

theKingInTatters :: CardDef
theKingInTatters = act "03324" "The King in Tatters" 3 DimCarcosa
