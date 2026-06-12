module Arkham.Agenda.Cards (module Arkham.Agenda.Cards, module X) where

import Arkham.Agenda.CardDefs.Base as X
import Arkham.Agenda.CardDefs.NightOfTheZealot as X
import Arkham.Agenda.CardDefs.TheDunwichLegacy as X
import Arkham.Agenda.CardDefs.ThePathToCarcosa as X
import Arkham.Agenda.CardDefs.TheForgottenAge as X
import Arkham.Agenda.CardDefs.TheCircleUndone as X
import Arkham.Agenda.CardDefs.TheDreamEaters as X
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy as X
import Arkham.Agenda.CardDefs.EdgeOfTheEarth as X
import Arkham.Agenda.CardDefs.TheScarletKeys as X
import Arkham.Agenda.CardDefs.TheFeastOfHemlockVale as X
import Arkham.Agenda.CardDefs.Core2026 as X
import Arkham.Agenda.CardDefs.ReturnTo as X
import Arkham.Agenda.CardDefs.Standalone as X
import Arkham.Agenda.CardDefs.ByTheBook as X
import Arkham.Agenda.CardDefs.AllOrNothing as X
import Arkham.Agenda.CardDefs.BadBlood as X
import Arkham.Agenda.CardDefs.LaidToRest as X
import Arkham.Agenda.CardDefs.EnthrallingEncore as X
import Arkham.Agenda.CardDefs.ReadOrDie as X

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Prelude hiding (fold)

allAgendaCards :: Map CardCode CardDef
allAgendaCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [ aBetrayalOfEyes
      , aCreatureOfTheBayou
      , agentsOfTheDark
      , agentsOfTheOuterGods
      , agonyAndDespairEpicMultiplayer
      , agonyAndDespair
      , aHarshWindBlows
      , aKillerParty
      , allBetsDown
      , allIsFullOfLove
      , allIsOne
      , annihilation
      , aroundTheTable
      , lambsToTheSlaughter
      , aSinisterRealm
      , aTearInReality
      , aTearInRealityV2
      , aTrailOfTwists
      , awakening
      , awakeningTheLabyrinthsOfLunacyEpicMultiplayer
      , awakeningTheLabyrinthsOfLunacy
      , backToTheVale
      , barricadedStreets
      , beckoningForPower
      , behindTheCurtain
      , besetByMonsters
      , betterNeverThanLate
      , bidingItsTime
      , breakingThrough
      , breakingThroughV2
      , brewingCatastropheV1
      , brewingCatastropheV2
      , brewingCatastropheV3
      , callingForthTheOldOnes
      , callOfMadness
      , celestialAlignment
      , chaosAtTheCarnevale
      , chaosIncarnate
      , chaosInTheCloverClub
      , cityOfBlood
      , cityOfTheGreatRace
      , coldWelcome
      , collidingRealities
      , confluxOfConsequence
      , crossroadsOfFate
      , curseOfTheAbyss
      , dangerousRide
      , darknessClosesIn
      , darkClouds
      , deadOfNight
      , deathsApproach
      , decrepitDecay
      , deepeningDark
      , desolationV1
      , desolationV2
      , doomFromBelow
      , drawnIn
      , easyPrey
      , eerieSilence
      , empireOfTheDead
      , empireOfTheUndead
      , emptyStreets
      , encore
      , endlessCaverns
      , endsAndMeans
      , etherealTangleV1
      , etherealTangleV2
      , etherealTangleV3
      , expeditionIntoTheWild
      , eyesInTheDark
      , eyesOfTheVoid
      , fashionablyLate
      , fearTheReaper
      , feedTheBeast
      , figuresInTheFog
      , floodedStreets
      , fogOnTheBay
      , forbiddenPeaks
      , franticPursuit
      , furyThatShakesTheEarth
      , gardenOfShadows
      , gnashingTeeth
      , growingSuspicion
      , hallsOfStMarys
      , hiddenEntanglements
      , hisDomain
      , horrificDescent
      , horrorsUnleashed
      , hospitalOfHorrors
      , hotPursuit
      , humanityFading
      , icyDepths
      , inEveryShadow
      , intoTheCaves
      , intoTheVoid
      , intoTheWhite
      , intruders
      , itAwaits
      , jessiesRequest
      , theMastermind
      , theSummoningOfSilenus
      , theSummoningProgresses
      , theSummoningNearsCompletion
      , theIncubationOfTheEgg
      , theIncubationProgresses
      , theIncubationNearsCompletion
      , theProliferationOfTheSwarm
      , theProliferationProgresses
      , theProliferationNearsCompletion
      , silenusDescends
      , theEggHatches
      , ezelZenRezlEmerges
      , journeyAcrossTheDreamlands
      , journeyThroughTheGates
      , judgementXX
      , justiceXI
      , letTheStormRageTheFloodBelow
      , letTheStormRageTheVortexAbove
      , livingWalls
      , lockedInside
      , lostAndForgotten
      , lostMemories
      , lurkingHorrors
      , madnessAndDeath
      , madnessCoils
      , madnessDies
      , madnessDrowns
      , manifestationsOfEvil
      , markedForSacrifice
      , maskedRevelers
      , openingHand
      , otherworldlyHorror
      , otherworldlyLambs
      , otherworldlySlaughter
      , outOfTime
      , overTheThreshold
      , painfulHistory
      , pastPresentAndFuture
      , pendulousThreads
      , plotsAndPanic
      , predatorOrPrey
      , quietHalls
      , rageOfTheDeep
      , rampagingCreatures
      , ransackingTheManor
      , realitiesInterwoven
      , relentlessTide
      , restlessDead
      , restrictedAccess
      , returnToPredatorOrPrey
      , riseOfTheGhouls
      , rollingBackwards
      , runningOutOfTime
      , runningRed
      , sacrificeForTheDeep
      , schemesInTheDarkBeyond
      , secretsBetterLeftHidden
      , secretsOfTheSeaV1
      , secretsOfTheSeaV2
      , seeingRed
      , settingSun
      , shadowsDeepen
      , showbusinessAsUsual
      , silenceSpeaks
      , silentStirring
      , sinkingGround
      , snappedThreads
      , somethingStirs
      , specialInvestigation
      , strangeDisappearances
      , swallowedSky
      , temperanceXIV
      , terrorAtFalconPoint
      , terrorDescends
      , thatWhichHasNoName
      , theAlarmIsRaised
      , theArkhamWoods
      , theBarrierIsThin
      , theBeastUnleashed
      , theBeatingHeart
      , theBoundaryBroken
      , theBridgeOfWebs
      , theBrotherhoodBidesTheirTime
      , theChariotVII
      , theChase
      , theChaseIsOnV1
      , theChaseIsOnV2
      , theChillOfNight
      , theCityFloods
      , theCloverClub
      , theConnection
      , theCurseSpreads
      , theDeadWithNoName
      , theDescentBegins
      , theDevilOfTheDepths
      , theEndOfAllThings
      , theEntityAboveTheFloodBelow
      , theEntityAboveTheVortexAbove
      , theEyeOfChaos
      , theFamiliar
      , theFestivitiesBegin
      , theFinalCountdown
      , theFinalMirage
      , theFirstNight
      , theFlood
      , theHangedManXII
      , theHermitIX
      , theHierophantV
      , theHouseAlwaysWatches
      , theHouseStirsV1
      , theHouseStirsV2
      , theHouseWithNoName
      , theInfestationSpreads
      , theInitiationV1
      , theInitiationV2
      , theJunglesHeart
      , theLonelyCaverns
      , theLoversVI
      , theMawWidens
      , theMiasmaBeckons
      , theMurder
      , theNightHowls
      , theOldOnesHunger
      , theOnslaught
      , theSilence
      , theMiasma
      , theSpiral
      , theRedDepths
      , theRitualAdvances
      , theRitualBegins
      , theRitualBeginsBlackStarsRise
      , theRougarouFeeds
      , theSealWeakens
      , theSecondNight
      , theShadowOfTheEclipse
      , theShapeOfChaos
      , theTempleWarden
      , theTerrifyingTruth
      , theThingInTheBog
      , theThingWithNoName
      , theThirdAct
      , theThirdNight
      , theTideRises
      , theTideRisesALightInTheFog
      , theTrueCulpritV1
      , theTrueCulpritV10
      , theTrueCulpritV2
      , theTrueCulpritV3
      , theTrueCulpritV4
      , theTrueCulpritV5
      , theTrueCulpritV6
      , theTrueCulpritV7
      , theTrueCulpritV8
      , theTrueCulpritV9
      , theTruthIsHidden
      , theTurn
      , theWaterRises
      , theWitchLight
      , theWorldUnbidden
      , theyAreUponYou
      , theyreGettingOut
      , threadsOfTime
      , threeFates
      , timeCollapsing
      , timeIsRunningShort
      , timeMarchesOn
      , torturousDescent
      , unchangingAsTheSea
      , undergroundMuscle
      , undergroundSurvey
      , underTheSurface
      , unexpectedGuests
      , unsettlingSilence
      , vengeance
      , vengeanceAwaits
      , welcomeToHemlockVale
      , whatLurksBelowV1
      , whatLurksBelowV2
      , whatsGoingOn
      , wheelOfFortuneX
      , whenItRains
      , whereIsShe
      , whereTheresSmoke
      --- Core2026
      --- Spreading Flames
      , pastCurfew
      , litUp
      , wildFlames
      --- Smoke and Mirrors
      , arkhamAlive
      , emergentEvils
      --- Queen of Ash
      , aGathering
      , aRitual
      , brethrenOfAsh
      , --- The Blob That Ate Everything
        theAnomalySpreads
      , theAnomalySwells
      , theAnomalyConsumes
      , --- By the Book
        aCovertConspiracy
      , yourDeadlineNears
      , --- All or Nothing
        eyesAllAroundYou
      , --- Bad Blood
        hyperboreanBlood
      , --- Laid to Rest
        gatheringMists
      , --- Enthralling Encore
        theHauntingOfTheWardTheatre
      , --- Read or Die
        mortalInquiry
      ]
