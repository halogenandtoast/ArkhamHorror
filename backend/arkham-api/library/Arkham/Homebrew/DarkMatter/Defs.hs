module Arkham.Homebrew.DarkMatter.Defs (module Arkham.Homebrew.DarkMatter.Defs) where

import Arkham.Card.CardDef
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.DarkMatter.Actions qualified as Actions
import Arkham.Homebrew.DarkMatter.Traits qualified as Traits

locations :: [CardDef]
locations =
  [ Locations.aHidingPlace
  , Locations.aMutiny
  , Locations.abandonedLander
  , Locations.adriftInSpace
  , Locations.airlocks
  , Locations.anAccident
  , Locations.biologyLab
  , Locations.bottomlessPit
  , Locations.brainStorage
  , Locations.cafeteria
  , Locations.cargoHold
  , Locations.cityOfCats
  , Locations.classroomK2
  , Locations.coldWastes
  , Locations.communicator
  , Locations.crewQuarters
  , Locations.cryosleepQuarters
  , Locations.crystalPeak
  , Locations.cyclopeanCaverns
  , Locations.derelictShip
  , Locations.dreamDiagnostics
  , Locations.earth
  , Locations.engineRoom_023
  , Locations.engineRoom_127
  , Locations.entranceHall
  , Locations.entranceTunnel
  , Locations.escapePodBay
  , Locations.feverDream
  , Locations.flightDeck
  , Locations.gymnasium
  , Locations.hiddenPassage
  , Locations.hope
  , Locations.hydroponics
  , Locations.iceCavity
  , Locations.iceSpires
  , Locations.impassableRavine
  , Locations.infirmary_025
  , Locations.infirmary_130
  , Locations.landingCraft
  , Locations.library
  , Locations.mainFacility
  , Locations.martianRuins
  , Locations.memoryScanner
  , Locations.messHall
  , Locations.moonbaseLaboratory
  , Locations.mountSinai
  , Locations.newBrooklyn
  , Locations.olympusTelescope
  , Locations.omniTransmitters
  , Locations.qCrystalMines
  , Locations.realitySimulator
  , Locations.schoolGrounds
  , Locations.schrodGenerators
  , Locations.shipMainframe
  , Locations.shipsBridge
  , Locations.stalagmiteForest
  , Locations.surfaceOfFragment
  , Locations.telecoms
  , Locations.theCassilda
  , Locations.theTatterdemalion
  , Locations.thresholdOfYuggoth
  , Locations.ventilationShaft
  , Locations.yuggoth
  ]

enemies :: [CardDef]
enemies =
  [ Enemies.caveDweller
  , Enemies.corruptedMachine
  , Enemies.cyberCultist
  , Enemies.cybervirus
  , Enemies.daemonOfNis
  , Enemies.domaagTeel
  , Enemies.exoroid
  , Enemies.glitchInTheSystem
  , Enemies.houndOfTindalos
  , Enemies.jv7Hyades
  , Enemies.lr02Hali
  , Enemies.manifestedWhispers
  , Enemies.martianCrab
  , Enemies.miGoSentinel
  , Enemies.miGoStabilizer
  , Enemies.mimic
  , Enemies.parasite
  , Enemies.quantumPhantom
  , Enemies.rats
  , Enemies.shadowOfThoughts
  , Enemies.shamblerFromTheStars
  , Enemies.sophisticSpires
  , Enemies.spacePirates
  , Enemies.spiritOfThan
  , Enemies.stalkingByakhee
  , Enemies.systemBug
  , Enemies.tassilda
  , Enemies.tatteredCurtains
  , Enemies.theBOOGEYMAN
  , Enemies.theEntity
  , Enemies.theFeasterFromAfar
  , Enemies.theGreys
  , Enemies.uplA21Demhe
  , Enemies.viciousByakhee
  , Enemies.virtualByakhee
  , Enemies.voidByakhee
  , Enemies.yellowMists
  , Enemies.yithianGuard
  , Enemies.yourOtherSelf
  ]

treacheries :: [CardDef]
treacheries =
  [ Treacheries.alienAid
  , Treacheries.alienation
  , Treacheries.allSeeingEye
  , Treacheries.anachronism
  , Treacheries.anothersWoe
  , Treacheries.artificialGravityMalfunction
  , Treacheries.brokenReality
  , Treacheries.cabinPressure
  , Treacheries.callOfTheVoid
  , Treacheries.caveCollapse
  , Treacheries.chosenByHim
  , Treacheries.closeEncounters
  , Treacheries.coldVacuum
  , Treacheries.comeCLOSER
  , Treacheries.contamination
  , Treacheries.coolantLeak
  , Treacheries.darkReflectionsMalingerer
  , Treacheries.darkReflectionsMurderer
  , Treacheries.darkReflectionsSycophant
  , Treacheries.darkReflectionsZealot
  , Treacheries.decoherence
  , Treacheries.decompression
  , Treacheries.delusionalMadness
  , Treacheries.digitalCorrosion
  , Treacheries.duplication
  , Treacheries.echoesOfTassildaMatter
  , Treacheries.echoesOfTassildaMind
  , Treacheries.electricSurge
  , Treacheries.entangled
  , Treacheries.extraterrestrialAssault
  , Treacheries.fathomlessRegrets
  , Treacheries.forbiddingPromises
  , Treacheries.fromTheDark
  , Treacheries.futureEvils
  , Treacheries.grimFuture
  , Treacheries.hallucinatoryHolograms
  , Treacheries.hastursDomain
  , Treacheries.hauntingPast
  , Treacheries.highRadiationLevels
  , Treacheries.hopeless
  , Treacheries.incomprehensible
  , Treacheries.infection
  , Treacheries.innocentMishap
  , Treacheries.irresistibleTruths
  , Treacheries.lostInTranslation
  , Treacheries.madnessOfCarcosa
  , Treacheries.miGoExperiments
  , Treacheries.micrometeoroid
  , Treacheries.nonEuclideanGeometry
  , Treacheries.paleBlueDot
  , Treacheries.paradoxicalThreat
  , Treacheries.perfectImitation
  , Treacheries.persistenceOfMemory
  , Treacheries.perspectiveSwitch
  , Treacheries.predictiveAlgorithm
  , Treacheries.quantumCollapse
  , Treacheries.radiantCrown
  , Treacheries.radioactiveDecay
  , Treacheries.rememberME
  , Treacheries.reminiscenceCovenant
  , Treacheries.reminiscencePledge
  , Treacheries.reminiscenceSecrets
  , Treacheries.sceneShifting
  , Treacheries.scrambled
  , Treacheries.simulationDiscrepancy
  , Treacheries.solarEclipse
  , Treacheries.solarFlare
  , Treacheries.songOfYourSoul
  , Treacheries.surprise
  , Treacheries.theColorsOfSpace
  , Treacheries.theDarkForest
  , Treacheries.theStarsWereRight
  , Treacheries.toxicPits
  , Treacheries.unstableDimension
  ]

playerTreacheries :: [CardDef]
playerTreacheries =
  [ Treacheries.desync
  ]

acts :: [CardDef]
acts =
  [ Acts.artificalInsanity
  , Acts.awakening
  , Acts.destabilization
  , Acts.elbrusStation
  , Acts.endTimes
  , Acts.eventHorizon
  , Acts.facingYourFears
  , Acts.firstEncounter
  , Acts.inLostCarcosa
  , Acts.isAnyoneHome
  , Acts.psychoanalysis
  , Acts.publicSchool187V10
  , Acts.publicSchool187V20
  , Acts.publicSchool187V30
  , Acts.quantumZeno
  , Acts.reconnected
  , Acts.saveOurSouls
  , Acts.secretsOfTheMind
  , Acts.tassildasAwakening
  , Acts.theHeirToCarcosa
  , Acts.theManInThePallidMask
  , Acts.theShadowOfEarth
  , Acts.theUnspeakableTruth
  , Acts.unmasked
  ]

agendas :: [CardDef]
agendas =
  [ Agendas.aNightmare
  , Agendas.againstTheSun
  , Agendas.darkMatter
  , Agendas.emergencyProcedure
  , Agendas.figmentOfYourImagination
  , Agendas.flightOfTheByakhees
  , Agendas.it
  , Agendas.itsWeirdAndPissedOff
  , Agendas.journeyAcrossSpace
  , Agendas.moonsOfSaturn
  , Agendas.outOfMind
  , Agendas.redSun
  , Agendas.riseOfTheMachines
  , Agendas.screamOfTheDead
  , Agendas.shallDryAndDie
  , Agendas.signsFromAldebaran
  , Agendas.supernova
  , Agendas.theGhostShip
  , Agendas.theNostalgiaII
  , Agendas.theQuantumMaelstrom_091
  , Agendas.theQuantumMaelstrom_092
  , Agendas.theQuantumMaelstrom_093
  , Agendas.theShadowsLengthen
  , Agendas.theThingFromEarth
  , Agendas.theThirdAct
  ]

encounterAssets :: [CardDef]
encounterAssets =
  [ Assets.adamTanner
  , Assets.arNO
  , Assets.bottleOfWhispers
  , Assets.brainCylinder089
  , Assets.brainCylinder114
  , Assets.brainCylinder367
  , Assets.captainBurr
  , Assets.directorCixin
  , Assets.doctorFeng
  , Assets.erwinSimmonsFading
  , Assets.erwinSimmonsQuantumPhysicist
  , Assets.evaSuit
  , Assets.heirToCarcosa
  , Assets.k11SurveyUnit
  , Assets.k2PS187100Functionality
  , Assets.k2PS18725Functionality
  , Assets.k2PS18750Functionality
  , Assets.k2PS18775Functionality
  , Assets.lastHope
  , Assets.ltArcherMichaels
  , Assets.maja
  , Assets.medicalFoam
  , Assets.miGoCollector
  , Assets.mindMachineInterface
  , Assets.muD12Mudbug
  , Assets.projectOrigami
  , Assets.radiationTablets
  , Assets.repairingTheThreshold
  , Assets.shieldingDevice
  , Assets.sophie
  , Assets.spaceArtillery
  , Assets.stasisCube
  , Assets.thePallidMask
  , Assets.universalArchives
  , Assets.virtualAccessKey
  ]

playerSkills :: [CardDef]
playerSkills = []

stories :: [CardDef]
stories =
  [ Stories.arrivalOfTheKing
  , Stories.delights
  , Stories.evidenceAdamTanner
  , Stories.evidenceCaptainBurr
  , Stories.evidenceDoctorFeng
  , Stories.evidenceLtArcherMichaels
  , Stories.evidenceMUD12Mudbug
  , Stories.evidenceSophie
  , Stories.finalDestination
  , Stories.forYouAlone
  , Stories.intoTheArchives
  , Stories.k2PS187CyberneticBrains
  , Stories.lostExpedition
  , Stories.reintegrated_062
  , Stories.reintegrated_063
  , Stories.reintegrated_064
  , Stories.reintegrated_065
  , Stories.ritualOfTheSun
  , Stories.strangeIsTheNight
  , Stories.theCultist
  , Stories.theMiner
  , Stories.theTeacher
  , Stories.whatTypeOfShipIsThis
  , Stories.whoAmI
  , Stories.withoutATrace
  ]

data DarkMatterDefs

instance IsHomebrewDefs DarkMatterDefs where
  homebrewDefs =
    HomebrewDefs
      { hdLocations = locations
      , hdEnemies = enemies
      , hdTreacheries = treacheries
      , hdPlayerTreacheries = playerTreacheries
      , hdActs = acts
      , hdAgendas = agendas
      , hdEncounterAssets = encounterAssets
      , hdPlayerSkills = playerSkills
      , hdStories = stories
      , hdTraits = Traits.traits
      , hdActions = Actions.actions
      , hdActionAffordability = Actions.actionAffordability
      }
