module Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries where

import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Homebrew.AgesUnwound.Traits
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

{- | The single-sided half of a card whose other face belongs to another card
type. @Arkham.Treachery.CardDefs.Base@ has no @otherSideIs@ of its own (the location,
story, enemy, act and agenda bases do), so this campaign declares one; the back
and the front each name the other, which is what keeps the card browser from
listing the same physical card twice.
-}
otherSideIs :: CardCode -> CardDef -> CardDef
otherSideIs cCode def = def {cdDoubleSided = False, cdOtherSide = Just cCode}

inEveryShadow :: CardDef
inEveryShadow =
  (treachery ":ages-unwound:020" "In Every Shadow" Set.NightOfFire 3)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

justBusiness :: CardDef
justBusiness =
  (treachery ":ages-unwound:021" "Just Business" Set.NightOfFire 3)
    { cdCardTraits = singleton Hazard
    }

sheerExhaustion :: CardDef
sheerExhaustion =
  (treachery ":ages-unwound:022" "Sheer Exhaustion" Set.NightOfFire 2)
    { cdCardTraits = singleton Terror
    }

gazeOfAforgomon :: CardDef
gazeOfAforgomon =
  (weakness ":ages-unwound:040" "Gaze of Aforgomon")
    { cdCardTraits = singleton Pact
    , cdEncounterSet = Just Set.TheMyriadGentleman
    , cdEncounterSetQuantity = Just 1
    }

exUnoPlures :: CardDef
exUnoPlures =
  (treachery ":ages-unwound:044" "Ex Uno Plures" Set.TheMyriadGentleman 2)
    { cdCardTraits = setFromList [Power, Myriad]
    }

theyJustKeepComing :: CardDef
theyJustKeepComing =
  (treachery ":ages-unwound:045" "They Just Keep Coming" Set.TheMyriadGentleman 2)
    { cdCardTraits = setFromList [Scheme, Myriad]
    }

notWelcomeHere :: CardDef
notWelcomeHere =
  (treachery ":ages-unwound:046" "Not Welcome Here" Set.TheMyriadGentleman 3)
    { cdCardTraits = setFromList [Scheme, Myriad]
    , cdKeywords = singleton Keyword.Peril
    }

fragmentedExistence :: CardDef
fragmentedExistence =
  (treachery ":ages-unwound:047" "Fragmented Existence" Set.TheMyriadGentleman 2)
    { cdCardTraits = singleton Paradox
    }

temporalStutter :: CardDef
temporalStutter =
  (treachery ":ages-unwound:061" "Temporal Stutter" Set.AWorldTornDown 3)
    { cdCardTraits = singleton Paradox
    }

timeRunsBackwards :: CardDef
timeRunsBackwards =
  (weakness ":ages-unwound:087" "Time Runs Backwards")
    { cdCardTraits = singleton Paradox
    , cdEncounterSet = Just Set.Unstuck
    , cdEncounterSetQuantity = Just 4
    }

aerialBombardment :: CardDef
aerialBombardment =
  (treachery ":ages-unwound:095" "Aerial Bombardment" Set.Unstuck 2)
    { cdCardTraits = singleton Hazard
    }

beckoningOfEverywhen :: CardDef
beckoningOfEverywhen =
  (treachery ":ages-unwound:096" "Beckoning of Everywhen" Set.Unstuck 3)
    { cdCardTraits = singleton Paradox
    }

cabinPressure :: CardDef
cabinPressure =
  (treachery ":ages-unwound:097" "Cabin Pressure" Set.Unstuck 2)
    { cdCardTraits = singleton Madness
    }

darkMaledict :: CardDef
darkMaledict =
  (treachery ":ages-unwound:098" "Dark Maledict" Set.Unstuck 2)
    { cdCardTraits = singleton Hex
    , cdKeywords = singleton Keyword.Peril
    }

dystopia :: CardDef
dystopia =
  (treachery ":ages-unwound:099" "Dystopia" Set.Unstuck 2)
    { cdCardTraits = singleton Terror
    }

morbidCuriosity :: CardDef
morbidCuriosity =
  (treachery ":ages-unwound:100" "Morbid Curiosity" Set.Unstuck 2)
    { cdCardTraits = singleton Terror
    }

overwhelmingNumbers :: CardDef
overwhelmingNumbers =
  (treachery ":ages-unwound:101" "Overwhelming Numbers" Set.Unstuck 2)
    { cdCardTraits = singleton Scheme
    }

romanOutpost :: CardDef
romanOutpost =
  (treachery ":ages-unwound:102" "Roman Outpost" Set.Unstuck 1)
    { cdCardTraits = singleton Scheme
    }

sandstorm :: CardDef
sandstorm =
  (treachery ":ages-unwound:103" "Sandstorm" Set.Unstuck 2)
    { cdCardTraits = singleton Hazard
    }

theEndlessFall :: CardDef
theEndlessFall =
  (treachery ":ages-unwound:104" "The Endless Fall" Set.Unstuck 3)
    { cdCardTraits = singleton Paradox
    }

curseOfAThousandWinters :: CardDef
curseOfAThousandWinters =
  (weakness ":ages-unwound:123" "Curse of a Thousand Winters")
    { cdCardTraits = singleton Curse
    , cdEncounterSet = Just Set.AYearToPlan
    , cdEncounterSetQuantity = Just 4
    }

aLongAndLonelyYear :: CardDef
aLongAndLonelyYear =
  (treachery ":ages-unwound:126" "A Long and Lonely Year" Set.AYearToPlan 2)
    { cdCardTraits = singleton Sorrow
    }

aMultitudeOfPlots :: CardDef
aMultitudeOfPlots =
  (treachery ":ages-unwound:127" "A Multitude of Plots" Set.AYearToPlan 3)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Peril
    }

withoutHomeOrIdentity :: CardDef
withoutHomeOrIdentity =
  (treachery ":ages-unwound:128" "Without Home or Identity" Set.AYearToPlan 2)
    { cdCardTraits = singleton Sorrow
    }

helpingYourself :: CardDef
helpingYourself =
  (treachery ":ages-unwound:129" "Helping Yourself" Set.Missions 1)
    { cdCardTraits = setFromList [Task, Paradox]
    }

enemyOfMyEnemy :: CardDef
enemyOfMyEnemy =
  (treachery ":ages-unwound:130" "Enemy of My Enemy" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

aTreasureUnearthed :: CardDef
aTreasureUnearthed =
  (treachery ":ages-unwound:131" "A Treasure Unearthed" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

theDevilYouKnow :: CardDef
theDevilYouKnow =
  (treachery ":ages-unwound:132" "The Devil You Know" Set.Missions 1)
    { cdCardTraits = setFromList [Task, SilverTwilight]
    }

higherPowers :: CardDef
higherPowers =
  (treachery ":ages-unwound:133" "Higher Powers" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

entreatingTheGods :: CardDef
entreatingTheGods =
  (treachery ":ages-unwound:134" "Entreating the Gods" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

upToSomething :: CardDef
upToSomething =
  (treachery ":ages-unwound:135" "Up To Something" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

contactingTheLodge :: CardDef
contactingTheLodge =
  otherSideIs ":ages-unwound:148b"
    $ (treachery ":ages-unwound:148" "Contacting the Lodge" Set.Missions 1)
      { cdCardTraits = setFromList [Scheme, SilverTwilight]
      }

exposition :: CardDef
exposition =
  otherSideIs ":ages-unwound:149b"
    $ (treachery ":ages-unwound:149" "Exposition" Set.Missions 1)
      { cdCardTraits = singleton Scheme
      }

finalPreparations :: CardDef
finalPreparations =
  (treachery ":ages-unwound:150" "Final Preparations" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

keeperOfKnowledge :: CardDef
keeperOfKnowledge =
  (treachery ":ages-unwound:151" "Keeper of Knowledge" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

schemeOfTheMyriad :: CardDef
schemeOfTheMyriad =
  otherSideIs ":ages-unwound:152b"
    $ (treachery ":ages-unwound:152" "Scheme of the Myriad" Set.Missions 1)
      { cdCardTraits = singleton Scheme
      }

strangePortal :: CardDef
strangePortal =
  (treachery ":ages-unwound:153" "Strange Portal" Set.Missions 1)
    { cdCardTraits = singleton Task
    }

theTunguskaEvent :: CardDef
theTunguskaEvent =
  otherSideIs ":ages-unwound:154b"
    $ (treachery ":ages-unwound:154" "The Tunguska Event" Set.Missions 1)
      { cdCardTraits = singleton Task
      }

nexusOfAforgomon :: CardDef
nexusOfAforgomon =
  (treachery ":ages-unwound:175" "Nexus of Aforgomon" Set.AWorldTornDownAgain 1)
    { cdCardTraits = singleton Power
    }

theColoursSpread :: CardDef
theColoursSpread =
  (treachery ":ages-unwound:176" "The Colour's Spread" Set.AWorldTornDownAgain 1)
    { cdCardTraits = singleton Power
    }

assistanceFromTheLodge :: CardDef
assistanceFromTheLodge =
  (treachery ":ages-unwound:177" "Assistance from the Lodge" Set.AWorldTornDownAgain 2)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Surge
    }

preserveCausality :: CardDef
preserveCausality =
  (treachery ":ages-unwound:179" "Preserve Causality" Set.AWorldTornDownAgain 2)
    { cdCardTraits = singleton Paradox
    , cdKeywords = singleton Keyword.Peril
    }

temporalInstability :: CardDef
temporalInstability =
  (treachery ":ages-unwound:180" "Temporal Instability" Set.AWorldTornDownAgain 3)
    { cdCardTraits = singleton Paradox
    }

youMustNotBeSeen :: CardDef
youMustNotBeSeen =
  (treachery ":ages-unwound:181" "You Must Not Be Seen" Set.AWorldTornDownAgain 2)
    { cdCardTraits = setFromList [Hazard, Paradox]
    , cdKeywords = singleton Keyword.Peril
    }

unwrittenExistence :: CardDef
unwrittenExistence =
  otherSideIs ":ages-unwound:192"
    $ (treachery ":ages-unwound:192b" "Unwritten Existence" Set.TimeRunsOut 1)
      { cdCardTraits = setFromList [Paradox, Fate]
      }

agedAThousandYears :: CardDef
agedAThousandYears =
  otherSideIs ":ages-unwound:198"
    $ (treachery ":ages-unwound:198b" "Aged a Thousand Years" Set.TimeRunsOut 1)
      { cdCardTraits = singleton Fate
      }

amIReal :: CardDef
amIReal =
  (treachery ":ages-unwound:213" "Am I... Real?" Set.TimeRunsOut 2)
    { cdCardTraits = singleton Fate
    , cdKeywords = singleton Keyword.Surge
    }

grandfatherParadox :: CardDef
grandfatherParadox =
  (treachery ":ages-unwound:214" "Grandfather Paradox" Set.TimeRunsOut 3)
    { cdCardTraits = singleton Paradox
    }

syzygy :: CardDef
syzygy =
  (treachery ":ages-unwound:215" "Syzygy" Set.TimeRunsOut 2)
    { cdCardTraits = singleton Power
    }

theChainOfAforgomon :: CardDef
theChainOfAforgomon =
  (treachery ":ages-unwound:216" "The Chain of Aforgomon" Set.TimeRunsOut 3)
    { cdCardTraits = setFromList [Power, Terror]
    }

watchYouBreak :: CardDef
watchYouBreak =
  (treachery ":ages-unwound:217" "Watch You Break" Set.TimeRunsOut 2)
    { cdCardTraits = singleton Power
    , cdKeywords = singleton Keyword.Peril
    }

weightOfTheCenturies :: CardDef
weightOfTheCenturies =
  (treachery ":ages-unwound:219" "Weight of the Centuries" Set.AgentsOfChronos 2)
    { cdCardTraits = singleton Hex
    }

omnipresence :: CardDef
omnipresence =
  (treachery ":ages-unwound:221" "Omnipresence" Set.Myriad 3)
    { cdCardTraits = setFromList [Scheme, Myriad]
    }

stirringTitan :: CardDef
stirringTitan =
  (treachery ":ages-unwound:234" "Stirring Titan" Set.NightOfTheRitual 2)
    { cdCardTraits = singleton Hazard
    , cdKeywords = singleton Keyword.Surge
    }

beckoningOfOblivion :: CardDef
beckoningOfOblivion =
  (treachery ":ages-unwound:236" "Beckoning of Oblivion" Set.NightOfTheRitual 2)
    { cdCardTraits = singleton Power
    }

timeLoop :: CardDef
timeLoop =
  (treachery ":ages-unwound:237" "Time Loop" Set.NightOfTheRitual 3)
    { cdCardTraits = singleton Paradox
    }

timeReshaped :: CardDef
timeReshaped =
  (treachery ":ages-unwound:238" "Time Reshaped" Set.NightOfTheRitual 3)
    { cdCardTraits = singleton Hex
    , cdKeywords = singleton Keyword.Surge
    }

timesEbb :: CardDef
timesEbb =
  (treachery ":ages-unwound:239" "Time's Ebb" Set.NightOfTheRitual 2)
    { cdCardTraits = singleton Power
    }

timesSurge :: CardDef
timesSurge =
  (treachery ":ages-unwound:240" "Time's Surge" Set.NightOfTheRitual 2)
    { cdCardTraits = singleton Power
    }

nightBlackAsPitch :: CardDef
nightBlackAsPitch =
  (treachery ":ages-unwound:241" "Night Black as Pitch" Set.Nyctophobia 2)
    { cdCardTraits = setFromList [Terror, Darkness]
    }

lostInTheDark :: CardDef
lostInTheDark =
  (treachery ":ages-unwound:242" "Lost in the Dark" Set.Nyctophobia 2)
    { cdCardTraits = setFromList [Terror, Darkness]
    }

realityUndone :: CardDef
realityUndone =
  (treachery ":ages-unwound:243" "Reality Undone" Set.Paradox 1)
    { cdCardTraits = singleton Paradox
    }

unevenAcceleration :: CardDef
unevenAcceleration =
  (treachery ":ages-unwound:244" "Uneven Acceleration" Set.Paradox 1)
    { cdCardTraits = singleton Paradox
    }

rewrittenExistence :: CardDef
rewrittenExistence =
  (treachery ":ages-unwound:245" "Rewritten Existence" Set.Paradox 2)
    { cdCardTraits = singleton Paradox
    }

aspectsOfInfinity :: CardDef
aspectsOfInfinity =
  (treachery ":ages-unwound:246" "Aspects of Infinity" Set.ShiftingReality 2)
    { cdCardTraits = singleton Power
    , cdKeywords = singleton Keyword.Surge
    }

outOfPhase :: CardDef
outOfPhase =
  (treachery ":ages-unwound:247" "Out of Phase" Set.ShiftingReality 2)
    { cdCardTraits = singleton Power
    }

skillsOfAnotherLife :: CardDef
skillsOfAnotherLife =
  (treachery ":ages-unwound:248" "Skills of Another Life" Set.ShiftingReality 2)
    { cdCardTraits = singleton Paradox
    }

riggedToBlow :: CardDef
riggedToBlow =
  (treachery ":ages-unwound:252" "Rigged to Blow" Set.Thugs 2)
    { cdCardTraits = singleton Hazard
    , cdKeywords = singleton Keyword.Peril
    }

unleashedChaosIAccelerationI :: CardDef
unleashedChaosIAccelerationI =
  (treachery ":ages-unwound:253" "Unleashed Chaos <i>(Acceleration)</i>" Set.UnleashedChaos 1)
    { cdCardTraits = singleton Power
    }

unleashedChaosIProliferationI :: CardDef
unleashedChaosIProliferationI =
  (treachery ":ages-unwound:254" "Unleashed Chaos <i>(Proliferation)</i>" Set.UnleashedChaos 1)
    { cdCardTraits = singleton Power
    }

unleashedChaosIMutationI :: CardDef
unleashedChaosIMutationI =
  (treachery ":ages-unwound:255" "Unleashed Chaos <i>(Mutation)</i>" Set.UnleashedChaos 1)
    { cdCardTraits = singleton Power
    }

memoriesFadeToDust :: CardDef
memoriesFadeToDust =
  (treachery ":ages-unwound:256" "Memories Fade to Dust" Set.UnravellingYears 3)
    { cdCardTraits = singleton Hex
    }

acceleratedDecay :: CardDef
acceleratedDecay =
  (treachery ":ages-unwound:257" "Accelerated Decay" Set.UnravellingYears 2)
    { cdCardTraits = singleton Hex
    }
