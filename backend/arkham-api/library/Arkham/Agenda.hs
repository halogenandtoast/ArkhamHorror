{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Agenda (
  module Arkham.Agenda,
) where

import Arkham.Prelude

import Arkham.Agenda.Agendas
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes

lookupAgenda :: AgendaId -> Int -> CardId -> Agenda
lookupAgenda agendaId = case lookup (unAgendaId agendaId) allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show agendaId
  Just (SomeAgendaCard a) -> \i cardId -> Agenda $ cbCardBuilder a cardId (i, agendaId)

instance RunMessage Agenda where
  runMessage msg (Agenda a) = Agenda <$> runMessage msg a

instance FromJSON Agenda where
  parseJSON = withObject "Agenda" $ \o -> do
    cCode <- o .: "id"
    withAgendaCardCode cCode
      $ \(_ :: AgendaCard a) -> Agenda <$> parseJSON @a (Object o)

withAgendaCardCode
  :: CardCode -> (forall a. IsAgenda a => AgendaCard a -> r) -> r
withAgendaCardCode cCode f = case lookup cCode allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show cCode
  Just (SomeAgendaCard a) -> f a

allAgendas :: Map CardCode SomeAgendaCard
allAgendas =
  mapFrom
    someAgendaCardCode
    [ -- Night of the Zealot
      -- The Gathering
      SomeAgendaCard whatsGoingOn
    , SomeAgendaCard riseOfTheGhouls
    , SomeAgendaCard theyreGettingOut
    , -- The Midnight Masks
      SomeAgendaCard predatorOrPrey
    , SomeAgendaCard timeIsRunningShort
    , -- The Devourer Below
      SomeAgendaCard theArkhamWoods
    , SomeAgendaCard theRitualBegins
    , SomeAgendaCard vengeanceAwaits
    , -- The Dunwich Legacy
      -- Extracurricular Activity
      SomeAgendaCard quietHalls
    , SomeAgendaCard deadOfNight
    , SomeAgendaCard theBeastUnleashed
    , -- The House Always Wins
      SomeAgendaCard theCloverClub
    , SomeAgendaCard undergroundMuscle
    , SomeAgendaCard chaosInTheCloverClub
    , -- The Miskatonic Museum
      SomeAgendaCard restrictedAccess
    , SomeAgendaCard shadowsDeepen
    , SomeAgendaCard inEveryShadow
    , -- The Essex County Express
      SomeAgendaCard aTearInReality
    , SomeAgendaCard theMawWidens
    , SomeAgendaCard rollingBackwards
    , SomeAgendaCard drawnIn
    , SomeAgendaCard outOfTime
    , -- Blood on the Altar
      SomeAgendaCard strangeDisappearances
    , SomeAgendaCard theOldOnesHunger
    , SomeAgendaCard feedTheBeast
    , -- Undimensioned and Unseen
      SomeAgendaCard rampagingCreatures
    , SomeAgendaCard bidingItsTime
    , SomeAgendaCard horrorsUnleashed
    , -- Where Doom Awaits
      SomeAgendaCard callingForthTheOldOnes
    , SomeAgendaCard beckoningForPower
    , -- Lost in Time and Space
      SomeAgendaCard allIsOne
    , SomeAgendaCard pastPresentAndFuture
    , SomeAgendaCard breakingThrough
    , SomeAgendaCard theEndOfAllThings
    , -- The Path to Carcosa
      -- Curtain Call
      SomeAgendaCard theThirdAct
    , SomeAgendaCard encore
    , -- The Last King
      SomeAgendaCard fashionablyLate
    , SomeAgendaCard theTerrifyingTruth
    , -- Echoes of the Past
      SomeAgendaCard theTruthIsHidden
    , SomeAgendaCard ransackingTheManor
    , SomeAgendaCard secretsBetterLeftHidden
    , -- The Unspeakable Oath
      SomeAgendaCard lockedInside
    , SomeAgendaCard torturousDescent
    , SomeAgendaCard hisDomain
    , -- A Phantom of Truth
      SomeAgendaCard theFirstNight
    , SomeAgendaCard theSecondNight
    , SomeAgendaCard theThirdNight
    , -- The Pallid Mask
      SomeAgendaCard empireOfTheDead
    , SomeAgendaCard empireOfTheUndead
    , -- Black Stars Rise
      SomeAgendaCard theTideRises
    , SomeAgendaCard letTheStormRageTheFloodBelow
    , SomeAgendaCard letTheStormRageTheVortexAbove
    , SomeAgendaCard theCityFloods
    , SomeAgendaCard theRitualBeginsBlackStarsRise
    , SomeAgendaCard theEntityAboveTheFloodBelow
    , SomeAgendaCard theEntityAboveTheVortexAbove
    , SomeAgendaCard swallowedSky
    , -- Dim Carcosa
      SomeAgendaCard madnessCoils
    , SomeAgendaCard madnessDrowns
    , SomeAgendaCard madnessDies
    , -- The Forgotten Age
      -- The Untamed Wilds
      SomeAgendaCard expeditionIntoTheWild
    , SomeAgendaCard intruders
    , -- The Doom of Eztli
      SomeAgendaCard somethingStirs
    , SomeAgendaCard theTempleWarden
    , -- Threads of Fate
      SomeAgendaCard threeFates
    , SomeAgendaCard behindTheCurtain
    , SomeAgendaCard hiddenEntanglements
    , -- The Boundary Beyond
      SomeAgendaCard theBoundaryBroken
    , SomeAgendaCard theBarrierIsThin
    , SomeAgendaCard timeCollapsing
    , -- Heart of the Elders
      --- Pillars of Judgement
      SomeAgendaCard theJunglesHeart
    , SomeAgendaCard settingSun
    , --- K'n-yan
      SomeAgendaCard theLonelyCaverns
    , SomeAgendaCard eyesInTheDark
    , -- The City of Archives
      SomeAgendaCard cityOfTheGreatRace
    , SomeAgendaCard lostMemories
    , SomeAgendaCard humanityFading
    , -- The Depth of Yoth
      SomeAgendaCard theDescentBegins
    , SomeAgendaCard horrificDescent
    , SomeAgendaCard endlessCaverns
    , SomeAgendaCard cityOfBlood
    , SomeAgendaCard furyThatShakesTheEarth
    , SomeAgendaCard theRedDepths
    , SomeAgendaCard vengeance
    , -- Shattered Aeons
      SomeAgendaCard threadsOfTime
    , SomeAgendaCard pendulousThreads
    , SomeAgendaCard snappedThreads
    , -- The Circle Undone
      -- Disappearance at the Twilight Estate
      SomeAgendaCard judgementXX
    , -- The Witching Hour
      SomeAgendaCard temperanceXIV
    , SomeAgendaCard theNightHowls
    , -- At Death's Doorstep
      SomeAgendaCard justiceXI
    , SomeAgendaCard overTheThreshold
    , -- The Secret Name
      SomeAgendaCard theHermitIX
    , SomeAgendaCard theFamiliar
    , SomeAgendaCard theWitchLight
    , SomeAgendaCard markedForSacrifice
    , -- The Wages of Sin
      SomeAgendaCard theHangedManXII
    , SomeAgendaCard deathsApproach
    , -- For the Greater Good
      SomeAgendaCard theHierophantV
    , SomeAgendaCard endsAndMeans
    , -- Union and Disillusion
      SomeAgendaCard theLoversVI
    , SomeAgendaCard crossroadsOfFate
    , -- In the Clutches of Chaos
      SomeAgendaCard theChariotVII
    , -- Before the Black Throne
      SomeAgendaCard wheelOfFortuneX
    , SomeAgendaCard itAwaits
    , SomeAgendaCard theFinalCountdown
    , -- The Dream-Eaters
      -- Beyond the Gates of Sleep
      SomeAgendaCard journeyThroughTheGates
    , -- Waking Nightmare
      SomeAgendaCard hallsOfStMarys
    , SomeAgendaCard theInfestationSpreads
    , SomeAgendaCard hospitalOfHorrors
    , SomeAgendaCard hallsOfStMarys
    , -- The Search for Kadath
      SomeAgendaCard journeyAcrossTheDreamlands
    , SomeAgendaCard agentsOfTheOuterGods
    , -- A Thousand Shapes of Horror
      SomeAgendaCard theHouseWithNoName
    , SomeAgendaCard theThingWithNoName
    , SomeAgendaCard theDeadWithNoName
    , -- Dark Side of the Moon
      SomeAgendaCard silentStirring
    , SomeAgendaCard theAlarmIsRaised
    , SomeAgendaCard theyAreUponYou
    , -- Point of No Return
      SomeAgendaCard aSinisterRealm
    , SomeAgendaCard besetByMonsters
    , -- Where the Gods Dwell
      SomeAgendaCard theEyeOfChaos
    , SomeAgendaCard theShapeOfChaos
    , SomeAgendaCard chaosIncarnate
    , -- Weaver of the Cosmos
      SomeAgendaCard theBridgeOfWebs
    , SomeAgendaCard aTrailOfTwists
    , SomeAgendaCard realitiesInterwoven
    , -- The Innsmouth Conspiracy
      --- The Pit of Despair
      SomeAgendaCard awakening
    , SomeAgendaCard theWaterRises
    , SomeAgendaCard sacrificeForTheDeep
    , --- The Vanishing of Elina Harper
      SomeAgendaCard decrepitDecay
    , SomeAgendaCard growingSuspicion
    , SomeAgendaCard franticPursuit
    , --- In Too Deep
      SomeAgendaCard barricadedStreets
    , SomeAgendaCard relentlessTide
    , SomeAgendaCard floodedStreets
    , SomeAgendaCard rageOfTheDeep
    , --- Devil Reef
      SomeAgendaCard secretsOfTheSeaV1
    , SomeAgendaCard secretsOfTheSeaV2
    , SomeAgendaCard theDevilOfTheDepths
    , --- Horror in High Gear
      SomeAgendaCard theChaseIsOnV1
    , SomeAgendaCard theChaseIsOnV2
    , SomeAgendaCard hotPursuit
    , --- A Light in the Fog
      SomeAgendaCard fogOnTheBay
    , SomeAgendaCard unchangingAsTheSea
    , SomeAgendaCard theTideRisesALightInTheFog
    , SomeAgendaCard terrorAtFalconPoint
    , --- The Lair of Dagon
      SomeAgendaCard theInitiationV1
    , SomeAgendaCard theInitiationV2
    , SomeAgendaCard whatLurksBelowV1
    , SomeAgendaCard whatLurksBelowV2
    , SomeAgendaCard theRitualAdvances
    , --- Into the Maelstrom
      SomeAgendaCard underTheSurface
    , SomeAgendaCard celestialAlignment
    , SomeAgendaCard theFlood
    , -- Edge of the Earth [eote]
      --- The Crash [eote]
      SomeAgendaCard coldWelcome
    , SomeAgendaCard intoTheWhite
    , SomeAgendaCard runningOutOfTime
    , --- Lost in the Night [eote]
      SomeAgendaCard aHarshWindBlows
    , SomeAgendaCard theChillOfNight
    , SomeAgendaCard madnessAndDeath
    , --- Seeping Nightmares [eote]
      SomeAgendaCard manifestationsOfEvil
    , SomeAgendaCard icyDepths
    , --- Fatal Mirage [eote]
      SomeAgendaCard etherealTangleV1
    , SomeAgendaCard etherealTangleV2
    , SomeAgendaCard etherealTangleV3
    , --- To the Forbidden Peaks [eote]
      SomeAgendaCard forbiddenPeaks
    , SomeAgendaCard terrorDescends
    , --- City of the Elder Things [eote]
      SomeAgendaCard lurkingHorrors
    , SomeAgendaCard doomFromBelow
    , --- The Great Seal [eote]
      SomeAgendaCard theBeatingHeart
    , SomeAgendaCard theMiasmaBeckons
    , SomeAgendaCard callOfMadness
    , --- Stirring in the Deep [eote]
      SomeAgendaCard theSealWeakens
    , SomeAgendaCard thatWhichHasNoName
    , -- Return to the Night of the Zealot
      -- Return to the Midnight Masks
      SomeAgendaCard returnToPredatorOrPrey
    , -- Return to the Dunwich Legacy
      --- Return to the Essex County Express
      SomeAgendaCard whereTheresSmoke
    , SomeAgendaCard aTearInRealityV2
    , --- Return to Lost in Time and Space
      SomeAgendaCard breakingThroughV2
    , -- Return to the Path to Carcosa
      --- Return to the LastKing
      SomeAgendaCard betterNeverThanLate
    , -- Curse of the Rougarou
      SomeAgendaCard aCreatureOfTheBayou
    , SomeAgendaCard theRougarouFeeds
    , SomeAgendaCard theCurseSpreads
    , -- Carnevale of Horrors
      SomeAgendaCard theFestivitiesBegin
    , SomeAgendaCard theShadowOfTheEclipse
    , SomeAgendaCard chaosAtTheCarnevale
    , -- Murder at the Excelsior Hotel
      SomeAgendaCard theMurder
    , SomeAgendaCard specialInvestigation
    , SomeAgendaCard theTrueCulpritV1
    , SomeAgendaCard theTrueCulpritV2
    , SomeAgendaCard theTrueCulpritV3
    , SomeAgendaCard theTrueCulpritV4
    , SomeAgendaCard theTrueCulpritV5
    , SomeAgendaCard theTrueCulpritV6
    , SomeAgendaCard theTrueCulpritV7
    , SomeAgendaCard theTrueCulpritV8
    , SomeAgendaCard theTrueCulpritV9
    , SomeAgendaCard theTrueCulpritV10
    , -- The Midwinter Gala
      SomeAgendaCard maskedRevelers
    , SomeAgendaCard unexpectedGuests
    , SomeAgendaCard aKillerParty
    ]
