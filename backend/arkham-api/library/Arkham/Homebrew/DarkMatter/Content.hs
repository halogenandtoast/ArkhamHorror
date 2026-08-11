module Arkham.Homebrew.DarkMatter.Content where

import Arkham.Act.Types (SomeActCard (..))
import Arkham.Agenda.Types (SomeAgendaCard (..))
import Arkham.Asset.Types (SomeAssetCard (..))
import Arkham.Card.CardCode
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard (..))
import Arkham.Homebrew.DarkMatter.Acts.ArtificalInsanity
import Arkham.Homebrew.DarkMatter.Acts.Awakening
import Arkham.Homebrew.DarkMatter.Acts.Destabilization
import Arkham.Homebrew.DarkMatter.Acts.ElbrusStation
import Arkham.Homebrew.DarkMatter.Acts.EndTimes
import Arkham.Homebrew.DarkMatter.Acts.EventHorizon
import Arkham.Homebrew.DarkMatter.Acts.FacingYourFears
import Arkham.Homebrew.DarkMatter.Acts.FirstEncounter
import Arkham.Homebrew.DarkMatter.Acts.InLostCarcosa
import Arkham.Homebrew.DarkMatter.Acts.IsAnyoneHome
import Arkham.Homebrew.DarkMatter.Acts.Psychoanalysis
import Arkham.Homebrew.DarkMatter.Acts.PublicSchool187
import Arkham.Homebrew.DarkMatter.Acts.QuantumZeno
import Arkham.Homebrew.DarkMatter.Acts.Reconnected
import Arkham.Homebrew.DarkMatter.Acts.SaveOurSouls
import Arkham.Homebrew.DarkMatter.Acts.SecretsOfTheMind
import Arkham.Homebrew.DarkMatter.Acts.TassildasAwakening
import Arkham.Homebrew.DarkMatter.Acts.TheHeirToCarcosa
import Arkham.Homebrew.DarkMatter.Acts.TheManInThePallidMask
import Arkham.Homebrew.DarkMatter.Acts.TheShadowOfEarth
import Arkham.Homebrew.DarkMatter.Acts.TheUnspeakableTruth
import Arkham.Homebrew.DarkMatter.Acts.Unmasked
import Arkham.Homebrew.DarkMatter.Agendas.DarkMatterAgenda
import Arkham.Homebrew.DarkMatter.Agendas.EmergencyProcedure
import Arkham.Homebrew.DarkMatter.Agendas.FigmentOfYourImagination
import Arkham.Homebrew.DarkMatter.Agendas.It
import Arkham.Homebrew.DarkMatter.Agendas.NostalgiaAgendas
import Arkham.Homebrew.DarkMatter.Agendas.RiseOfTheMachines
import Arkham.Homebrew.DarkMatter.Agendas.SaturnAgendas
import Arkham.Homebrew.DarkMatter.Agendas.ShallDryAndDie
import Arkham.Homebrew.DarkMatter.Agendas.StarfallAgendas
import Arkham.Homebrew.DarkMatter.Agendas.TheGhostShip
import Arkham.Homebrew.DarkMatter.Agendas.TheMachineInYellowAgendas
import Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstrom
import Arkham.Homebrew.DarkMatter.Agendas.TheShadowsLengthen
import Arkham.Homebrew.DarkMatter.Assets.AdamTanner
import Arkham.Homebrew.DarkMatter.Assets.BottleOfWhispers
import Arkham.Homebrew.DarkMatter.Assets.BrainCylinders
import Arkham.Homebrew.DarkMatter.Assets.CaptainBurr
import Arkham.Homebrew.DarkMatter.Assets.DoctorFeng
import Arkham.Homebrew.DarkMatter.Assets.ErwinSimmonsFading
import Arkham.Homebrew.DarkMatter.Assets.ErwinSimmonsQuantumPhysicist
import Arkham.Homebrew.DarkMatter.Assets.EvaSuit
import Arkham.Homebrew.DarkMatter.Assets.HeirToCarcosa
import Arkham.Homebrew.DarkMatter.Assets.K11SurveyUnit
import Arkham.Homebrew.DarkMatter.Assets.K2PS187100Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18725Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18750Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18775Functionality
import Arkham.Homebrew.DarkMatter.Assets.LtArcherMichaels
import Arkham.Homebrew.DarkMatter.Assets.MUD12Mudbug
import Arkham.Homebrew.DarkMatter.Assets.Maja
import Arkham.Homebrew.DarkMatter.Assets.MedicalFoam
import Arkham.Homebrew.DarkMatter.Assets.MindMachineInterface
import Arkham.Homebrew.DarkMatter.Assets.RadiationTablets
import Arkham.Homebrew.DarkMatter.Assets.ReintegrationChildren
import Arkham.Homebrew.DarkMatter.Assets.ShieldingDevice
import Arkham.Homebrew.DarkMatter.Assets.Sophie
import Arkham.Homebrew.DarkMatter.Assets.SpaceArtillery
import Arkham.Homebrew.DarkMatter.Assets.StarfallContacts
import Arkham.Homebrew.DarkMatter.Assets.StarfallObjectives
import Arkham.Homebrew.DarkMatter.Assets.StasisCube
import Arkham.Homebrew.DarkMatter.Assets.ThePallidMask
import Arkham.Homebrew.DarkMatter.Assets.UniversalArchives
import Arkham.Homebrew.DarkMatter.Assets.VirtualAccessKey
import Arkham.Homebrew.DarkMatter.Campaign (darkMatter)
import Arkham.Homebrew.DarkMatter.Enemies.CaveDweller
import Arkham.Homebrew.DarkMatter.Enemies.CorruptedMachine
import Arkham.Homebrew.DarkMatter.Enemies.CyberCultist
import Arkham.Homebrew.DarkMatter.Enemies.Cybervirus
import Arkham.Homebrew.DarkMatter.Enemies.DaemonOfNis
import Arkham.Homebrew.DarkMatter.Enemies.DomaagTeel
import Arkham.Homebrew.DarkMatter.Enemies.Exoroid
import Arkham.Homebrew.DarkMatter.Enemies.FlightOfTheByakhees
import Arkham.Homebrew.DarkMatter.Enemies.GlitchInTheSystem
import Arkham.Homebrew.DarkMatter.Enemies.HoundOfTindalos
import Arkham.Homebrew.DarkMatter.Enemies.Jv7Hyades
import Arkham.Homebrew.DarkMatter.Enemies.Lr02Hali
import Arkham.Homebrew.DarkMatter.Enemies.ManifestedWhispers
import Arkham.Homebrew.DarkMatter.Enemies.MartianCrab
import Arkham.Homebrew.DarkMatter.Enemies.MiGoScientist
import Arkham.Homebrew.DarkMatter.Enemies.MiGoSentinel
import Arkham.Homebrew.DarkMatter.Enemies.MiGoStabilizer
import Arkham.Homebrew.DarkMatter.Enemies.Mimic
import Arkham.Homebrew.DarkMatter.Enemies.Parasite
import Arkham.Homebrew.DarkMatter.Enemies.QuantumPhantom
import Arkham.Homebrew.DarkMatter.Enemies.Rats
import Arkham.Homebrew.DarkMatter.Enemies.ShadowOfThoughts
import Arkham.Homebrew.DarkMatter.Enemies.ShamblerFromTheStars
import Arkham.Homebrew.DarkMatter.Enemies.SophisticSpires
import Arkham.Homebrew.DarkMatter.Enemies.SpacePirates
import Arkham.Homebrew.DarkMatter.Enemies.SpiritOfThan
import Arkham.Homebrew.DarkMatter.Enemies.StalkingByakhee
import Arkham.Homebrew.DarkMatter.Enemies.SystemBug
import Arkham.Homebrew.DarkMatter.Enemies.Tassilda
import Arkham.Homebrew.DarkMatter.Enemies.TatteredCurtains
import Arkham.Homebrew.DarkMatter.Enemies.TheBOOGEYMAN
import Arkham.Homebrew.DarkMatter.Enemies.TheEntity
import Arkham.Homebrew.DarkMatter.Enemies.TheFeasterFromAfar
import Arkham.Homebrew.DarkMatter.Enemies.TheGreys
import Arkham.Homebrew.DarkMatter.Enemies.TheStranger
import Arkham.Homebrew.DarkMatter.Enemies.UplA21Demhe
import Arkham.Homebrew.DarkMatter.Enemies.ViciousByakhee
import Arkham.Homebrew.DarkMatter.Enemies.VirtualByakhee
import Arkham.Homebrew.DarkMatter.Enemies.VoidByakhee
import Arkham.Homebrew.DarkMatter.Enemies.YellowMists
import Arkham.Homebrew.DarkMatter.Enemies.YithianGuard
import Arkham.Homebrew.DarkMatter.Enemies.YourOtherSelf
import Arkham.Homebrew.DarkMatter.Locations.AHidingPlace
import Arkham.Homebrew.DarkMatter.Locations.AMutiny
import Arkham.Homebrew.DarkMatter.Locations.AbandonedLander
import Arkham.Homebrew.DarkMatter.Locations.AdriftInSpace
import Arkham.Homebrew.DarkMatter.Locations.Airlocks
import Arkham.Homebrew.DarkMatter.Locations.AnAccident
import Arkham.Homebrew.DarkMatter.Locations.BiologyLab
import Arkham.Homebrew.DarkMatter.Locations.BottomlessPit
import Arkham.Homebrew.DarkMatter.Locations.BrainStorage
import Arkham.Homebrew.DarkMatter.Locations.Cafeteria
import Arkham.Homebrew.DarkMatter.Locations.CargoHold
import Arkham.Homebrew.DarkMatter.Locations.CityOfCats
import Arkham.Homebrew.DarkMatter.Locations.ClassroomK2
import Arkham.Homebrew.DarkMatter.Locations.ColdWastes
import Arkham.Homebrew.DarkMatter.Locations.Communicator
import Arkham.Homebrew.DarkMatter.Locations.CrewQuarters
import Arkham.Homebrew.DarkMatter.Locations.CryosleepQuarters
import Arkham.Homebrew.DarkMatter.Locations.CrystalPeak
import Arkham.Homebrew.DarkMatter.Locations.CyclopeanCaverns
import Arkham.Homebrew.DarkMatter.Locations.DerelictShip
import Arkham.Homebrew.DarkMatter.Locations.DreamDiagnostics
import Arkham.Homebrew.DarkMatter.Locations.Earth
import Arkham.Homebrew.DarkMatter.Locations.EngineRoomInTheShadowOfEarth
import Arkham.Homebrew.DarkMatter.Locations.EngineRoomTatterdemalion
import Arkham.Homebrew.DarkMatter.Locations.EntranceHall
import Arkham.Homebrew.DarkMatter.Locations.EntranceTunnel
import Arkham.Homebrew.DarkMatter.Locations.EscapePodBay
import Arkham.Homebrew.DarkMatter.Locations.FeverDream
import Arkham.Homebrew.DarkMatter.Locations.FlightDeck
import Arkham.Homebrew.DarkMatter.Locations.GardensOfThothut
import Arkham.Homebrew.DarkMatter.Locations.GrandBallroom
import Arkham.Homebrew.DarkMatter.Locations.Gymnasium
import Arkham.Homebrew.DarkMatter.Locations.HiddenPassage
import Arkham.Homebrew.DarkMatter.Locations.Hope
import Arkham.Homebrew.DarkMatter.Locations.Hydroponics
import Arkham.Homebrew.DarkMatter.Locations.IceCavity
import Arkham.Homebrew.DarkMatter.Locations.IceSpires
import Arkham.Homebrew.DarkMatter.Locations.ImpassableRavine
import Arkham.Homebrew.DarkMatter.Locations.InfirmaryInTheShadowOfEarth
import Arkham.Homebrew.DarkMatter.Locations.InfirmaryTatterdemalion
import Arkham.Homebrew.DarkMatter.Locations.LabyrinthsOfTasylock
import Arkham.Homebrew.DarkMatter.Locations.LandingCraft
import Arkham.Homebrew.DarkMatter.Locations.Library
import Arkham.Homebrew.DarkMatter.Locations.MainFacility
import Arkham.Homebrew.DarkMatter.Locations.MartianRuins
import Arkham.Homebrew.DarkMatter.Locations.MemoryScanner
import Arkham.Homebrew.DarkMatter.Locations.MessHall
import Arkham.Homebrew.DarkMatter.Locations.MoonbaseLaboratory
import Arkham.Homebrew.DarkMatter.Locations.MountSinai
import Arkham.Homebrew.DarkMatter.Locations.NewBrooklyn
import Arkham.Homebrew.DarkMatter.Locations.OlympusTelescope
import Arkham.Homebrew.DarkMatter.Locations.OmniTransmitters
import Arkham.Homebrew.DarkMatter.Locations.PalaceGates
import Arkham.Homebrew.DarkMatter.Locations.QCrystalMines
import Arkham.Homebrew.DarkMatter.Locations.RealitySimulator
import Arkham.Homebrew.DarkMatter.Locations.SchoolGrounds
import Arkham.Homebrew.DarkMatter.Locations.SchrodGenerators
import Arkham.Homebrew.DarkMatter.Locations.ShipMainframe
import Arkham.Homebrew.DarkMatter.Locations.ShipsBridge
import Arkham.Homebrew.DarkMatter.Locations.Sol
import Arkham.Homebrew.DarkMatter.Locations.SpiralStaircase
import Arkham.Homebrew.DarkMatter.Locations.StalagmiteForest
import Arkham.Homebrew.DarkMatter.Locations.Starships
import Arkham.Homebrew.DarkMatter.Locations.SurfaceOfFragment
import Arkham.Homebrew.DarkMatter.Locations.Telecoms
import Arkham.Homebrew.DarkMatter.Locations.TheYellowThrone
import Arkham.Homebrew.DarkMatter.Locations.ThresholdOfYuggoth
import Arkham.Homebrew.DarkMatter.Locations.VentilationShaft
import Arkham.Homebrew.DarkMatter.Locations.Yuggoth
import Arkham.Homebrew.DarkMatter.Scenarios.ElectricNightmare
import Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa
import Arkham.Homebrew.DarkMatter.Scenarios.InTheShadowOfEarth
import Arkham.Homebrew.DarkMatter.Scenarios.LostQuantum
import Arkham.Homebrew.DarkMatter.Scenarios.Starfall
import Arkham.Homebrew.DarkMatter.Scenarios.StrangeMoons
import Arkham.Homebrew.DarkMatter.Scenarios.TheMachineInYellow
import Arkham.Homebrew.DarkMatter.Scenarios.TheTatterdemalion
import Arkham.Homebrew.DarkMatter.Sets qualified as Sets
import Arkham.Homebrew.DarkMatter.Stories.ArrivalOfTheKing
import Arkham.Homebrew.DarkMatter.Stories.Delights
import Arkham.Homebrew.DarkMatter.Stories.Evidence
import Arkham.Homebrew.DarkMatter.Stories.FinalDestination
import Arkham.Homebrew.DarkMatter.Stories.ForYouAlone
import Arkham.Homebrew.DarkMatter.Stories.IntoTheArchives
import Arkham.Homebrew.DarkMatter.Stories.K2PS187CyberneticBrains
import Arkham.Homebrew.DarkMatter.Stories.LostExpedition
import Arkham.Homebrew.DarkMatter.Stories.Reintegrated
import Arkham.Homebrew.DarkMatter.Stories.RitualOfTheSun
import Arkham.Homebrew.DarkMatter.Stories.StrangeIsTheNight
import Arkham.Homebrew.DarkMatter.Stories.TheCultist
import Arkham.Homebrew.DarkMatter.Stories.TheMiner
import Arkham.Homebrew.DarkMatter.Stories.TheTeacher
import Arkham.Homebrew.DarkMatter.Stories.WhatTypeOfShipIsThis
import Arkham.Homebrew.DarkMatter.Stories.WhoAmI
import Arkham.Homebrew.DarkMatter.Stories.WithoutATrace
import Arkham.Homebrew.DarkMatter.Treacheries.AlienAid
import Arkham.Homebrew.DarkMatter.Treacheries.Alienation
import Arkham.Homebrew.DarkMatter.Treacheries.AllSeeingEye
import Arkham.Homebrew.DarkMatter.Treacheries.Anachronism
import Arkham.Homebrew.DarkMatter.Treacheries.AnothersWoe
import Arkham.Homebrew.DarkMatter.Treacheries.ArtificialGravityMalfunction
import Arkham.Homebrew.DarkMatter.Treacheries.BrokenReality
import Arkham.Homebrew.DarkMatter.Treacheries.CabinPressure
import Arkham.Homebrew.DarkMatter.Treacheries.CallOfTheVoid
import Arkham.Homebrew.DarkMatter.Treacheries.CaveCollapse
import Arkham.Homebrew.DarkMatter.Treacheries.ChosenByHim
import Arkham.Homebrew.DarkMatter.Treacheries.CloseEncounters
import Arkham.Homebrew.DarkMatter.Treacheries.ColdVacuum
import Arkham.Homebrew.DarkMatter.Treacheries.ComeCLOSER
import Arkham.Homebrew.DarkMatter.Treacheries.Contamination
import Arkham.Homebrew.DarkMatter.Treacheries.CoolantLeak
import Arkham.Homebrew.DarkMatter.Treacheries.DarkReflections
import Arkham.Homebrew.DarkMatter.Treacheries.Decoherence
import Arkham.Homebrew.DarkMatter.Treacheries.Decompression
import Arkham.Homebrew.DarkMatter.Treacheries.DelusionalMadness
import Arkham.Homebrew.DarkMatter.Treacheries.Desync
import Arkham.Homebrew.DarkMatter.Treacheries.DigitalCorrosion
import Arkham.Homebrew.DarkMatter.Treacheries.Duplication
import Arkham.Homebrew.DarkMatter.Treacheries.EchoesOfTassilda
import Arkham.Homebrew.DarkMatter.Treacheries.ElectricSurge
import Arkham.Homebrew.DarkMatter.Treacheries.Entangled
import Arkham.Homebrew.DarkMatter.Treacheries.ExtraterrestrialAssault
import Arkham.Homebrew.DarkMatter.Treacheries.FathomlessRegrets
import Arkham.Homebrew.DarkMatter.Treacheries.ForbiddingPromises
import Arkham.Homebrew.DarkMatter.Treacheries.FromTheDark
import Arkham.Homebrew.DarkMatter.Treacheries.FutureEvils
import Arkham.Homebrew.DarkMatter.Treacheries.GrimFuture
import Arkham.Homebrew.DarkMatter.Treacheries.HallucinatoryHolograms
import Arkham.Homebrew.DarkMatter.Treacheries.HastursDomain
import Arkham.Homebrew.DarkMatter.Treacheries.HauntingPast
import Arkham.Homebrew.DarkMatter.Treacheries.HighRadiationLevels
import Arkham.Homebrew.DarkMatter.Treacheries.Hopeless
import Arkham.Homebrew.DarkMatter.Treacheries.Incomprehensible
import Arkham.Homebrew.DarkMatter.Treacheries.Infection
import Arkham.Homebrew.DarkMatter.Treacheries.InnocentMishap
import Arkham.Homebrew.DarkMatter.Treacheries.IrresistibleTruths
import Arkham.Homebrew.DarkMatter.Treacheries.LostInTranslation
import Arkham.Homebrew.DarkMatter.Treacheries.MadnessOfCarcosa
import Arkham.Homebrew.DarkMatter.Treacheries.MiGoExperiments
import Arkham.Homebrew.DarkMatter.Treacheries.Micrometeoroid
import Arkham.Homebrew.DarkMatter.Treacheries.NonEuclideanGeometry
import Arkham.Homebrew.DarkMatter.Treacheries.PaleBlueDot
import Arkham.Homebrew.DarkMatter.Treacheries.ParadoxicalThreat
import Arkham.Homebrew.DarkMatter.Treacheries.PerfectImitation
import Arkham.Homebrew.DarkMatter.Treacheries.PersistenceOfMemory
import Arkham.Homebrew.DarkMatter.Treacheries.PerspectiveSwitch
import Arkham.Homebrew.DarkMatter.Treacheries.PredictiveAlgorithm
import Arkham.Homebrew.DarkMatter.Treacheries.QuantumCollapse
import Arkham.Homebrew.DarkMatter.Treacheries.RadiantCrown
import Arkham.Homebrew.DarkMatter.Treacheries.RadioactiveDecay
import Arkham.Homebrew.DarkMatter.Treacheries.RememberME
import Arkham.Homebrew.DarkMatter.Treacheries.Reminiscence
import Arkham.Homebrew.DarkMatter.Treacheries.SceneShifting
import Arkham.Homebrew.DarkMatter.Treacheries.Scrambled
import Arkham.Homebrew.DarkMatter.Treacheries.SimulationDiscrepancy
import Arkham.Homebrew.DarkMatter.Treacheries.SolarEclipse
import Arkham.Homebrew.DarkMatter.Treacheries.SolarFlare
import Arkham.Homebrew.DarkMatter.Treacheries.SongOfYourSoul
import Arkham.Homebrew.DarkMatter.Treacheries.Surprise
import Arkham.Homebrew.DarkMatter.Treacheries.TheColorsOfSpace
import Arkham.Homebrew.DarkMatter.Treacheries.TheDarkForest
import Arkham.Homebrew.DarkMatter.Treacheries.TheStarsWereRight
import Arkham.Homebrew.DarkMatter.Treacheries.ToxicPits
import Arkham.Homebrew.DarkMatter.Treacheries.UnstableDimension
import Arkham.Homebrew.Types
import Arkham.Id (CampaignId)
import Arkham.Location.Types (SomeLocationCard (..))
import Arkham.Story.Types (SomeStoryCard (..))
import Arkham.Treachery.Types (SomeTreacheryCard (..))

acts :: [SomeActCard]
acts =
  [ SomeActCard eventHorizon
  , SomeActCard artificalInsanity
  , SomeActCard reconnected
  , SomeActCard publicSchool187V10
  , SomeActCard publicSchool187V20
  , SomeActCard publicSchool187V30
  , SomeActCard firstEncounter
  , SomeActCard inLostCarcosa
  , SomeActCard awakening
  , SomeActCard theManInThePallidMask
  , SomeActCard unmasked
  , SomeActCard endTimes
  , SomeActCard tassildasAwakening
  , SomeActCard theHeirToCarcosa
  , SomeActCard theUnspeakableTruth
  , SomeActCard isAnyoneHome
  , SomeActCard psychoanalysis
  , SomeActCard destabilization
  , SomeActCard saveOurSouls
  , SomeActCard secretsOfTheMind
  , SomeActCard theShadowOfEarth
  , SomeActCard elbrusStation
  , SomeActCard quantumZeno
  , SomeActCard facingYourFears
  ]

agendas :: [SomeAgendaCard]
agendas =
  [ SomeAgendaCard emergencyProcedure
  , SomeAgendaCard theGhostShip
  , SomeAgendaCard riseOfTheMachines
  , SomeAgendaCard figmentOfYourImagination
  , SomeAgendaCard it
  , SomeAgendaCard theShadowsLengthen
  , SomeAgendaCard shallDryAndDie
  , SomeAgendaCard moonsOfSaturn
  , SomeAgendaCard darkMatterAgenda
  , SomeAgendaCard theThirdAct
  , SomeAgendaCard aNightmare
  , SomeAgendaCard outOfMind
  , SomeAgendaCard journeyAcrossSpace
  , SomeAgendaCard redSun
  , SomeAgendaCard supernova
  , SomeAgendaCard signsFromAldebaran
  , SomeAgendaCard flightOfTheByakhees
  , SomeAgendaCard againstTheSun
  , SomeAgendaCard theNostalgiaII
  , SomeAgendaCard theThingFromEarth
  , SomeAgendaCard screamOfTheDead
  , SomeAgendaCard itsWeirdAndPissedOff
  , SomeAgendaCard theQuantumMaelstrom_091
  , SomeAgendaCard theQuantumMaelstrom_092
  , SomeAgendaCard theQuantumMaelstrom_093
  ]

assets :: [SomeAssetCard]
assets =
  [ SomeAssetCard virtualAccessKey
  , SomeAssetCard doctorFeng
  , SomeAssetCard muD12Mudbug
  , SomeAssetCard bottleOfWhispers
  , SomeAssetCard erwinSimmonsFading
  , SomeAssetCard k11SurveyUnit
  , SomeAssetCard universalArchives
  , SomeAssetCard sophie
  , SomeAssetCard captainBurr
  , SomeAssetCard shieldingDevice
  , SomeAssetCard stasisCube
  , SomeAssetCard projectOrigami
  , SomeAssetCard lastHope
  , SomeAssetCard repairingTheThreshold
  , SomeAssetCard arNO
  , SomeAssetCard directorCixin
  , SomeAssetCard miGoCollector
  , SomeAssetCard brainCylinder089
  , SomeAssetCard brainCylinder114
  , SomeAssetCard brainCylinder367
  , SomeAssetCard adamTanner
  , SomeAssetCard thePallidMask
  , SomeAssetCard erwinSimmonsQuantumPhysicist
  , SomeAssetCard evaSuit
  , SomeAssetCard heirToCarcosa
  , SomeAssetCard ltArcherMichaels
  , SomeAssetCard medicalFoam
  , SomeAssetCard mindMachineInterface
  , SomeAssetCard radiationTablets
  , SomeAssetCard alma
  , SomeAssetCard david
  , SomeAssetCard tilde
  , SomeAssetCard william
  , SomeAssetCard spaceArtillery
  , SomeAssetCard maja
  , SomeAssetCard k2PS187100Functionality
  , SomeAssetCard k2PS18725Functionality
  , SomeAssetCard k2PS18750Functionality
  , SomeAssetCard k2PS18775Functionality
  ]

enemies :: [SomeEnemyCard]
enemies =
  [ SomeEnemyCard corruptedMachine
  , SomeEnemyCard cybervirus
  , SomeEnemyCard glitchInTheSystem
  , SomeEnemyCard jv7Hyades
  , SomeEnemyCard lr02Hali
  , SomeEnemyCard manifestedWhispers
  , SomeEnemyCard miGoStabilizer
  , SomeEnemyCard quantumPhantom
  , SomeEnemyCard rats
  , SomeEnemyCard shadowOfThoughts
  , SomeEnemyCard miGoScientist
  , SomeEnemyCard theEntity
  , SomeEnemyCard tassilda
  , SomeEnemyCard haita
  , SomeEnemyCard miGoSentinel
  , SomeEnemyCard shamblerFromTheStars
  , SomeEnemyCard yourOtherSelf
  , SomeEnemyCard theStranger
  , SomeEnemyCard daemonOfNis
  , SomeEnemyCard spiritOfThan
  , SomeEnemyCard houndOfTindalos
  , SomeEnemyCard mimic
  , SomeEnemyCard voidByakhee
  , SomeEnemyCard martianCrab
  , SomeEnemyCard sophisticSpires
  , SomeEnemyCard spacePirates
  , SomeEnemyCard tatteredCurtains
  , SomeEnemyCard caveDweller
  , SomeEnemyCard cyberCultist
  , SomeEnemyCard domaagTeel
  , SomeEnemyCard exoroid
  , SomeEnemyCard yellowMists
  , SomeEnemyCard yithianGuard
  , SomeEnemyCard theGreys
  , SomeEnemyCard parasite
  , SomeEnemyCard stalkingByakhee
  , SomeEnemyCard systemBug
  , SomeEnemyCard theBOOGEYMAN
  , SomeEnemyCard theFeasterFromAfar
  , SomeEnemyCard uplA21Demhe
  , SomeEnemyCard viciousByakhee
  , SomeEnemyCard virtualByakhee
  ]

locations :: [SomeLocationCard]
locations =
  [ SomeLocationCard aHidingPlace
  , SomeLocationCard aMutiny
  , SomeLocationCard airlocks
  , SomeLocationCard anAccident
  , SomeLocationCard biologyLab
  , SomeLocationCard cafeteria
  , SomeLocationCard mountSinai
  , SomeLocationCard thresholdOfYuggoth
  , SomeLocationCard sol
  , SomeLocationCard theTatterdemalionLocation
  , SomeLocationCard theCassilda
  , SomeLocationCard shipMainframe
  , SomeLocationCard telecoms
  , SomeLocationCard yuggoth
  , SomeLocationCard olympusTelescope
  , SomeLocationCard derelictShip
  , SomeLocationCard moonbaseLaboratory
  , SomeLocationCard adriftInSpace
  , SomeLocationCard earth
  , SomeLocationCard hope
  , SomeLocationCard martianRuins
  , SomeLocationCard newBrooklyn
  , SomeLocationCard abandonedLander
  , SomeLocationCard bottomlessPit
  , SomeLocationCard cyclopeanCaverns
  , SomeLocationCard gardensOfThothut
  , SomeLocationCard grandBallroom
  , SomeLocationCard hiddenPassage
  , SomeLocationCard iceCavity
  , SomeLocationCard impassableRavine
  , SomeLocationCard labyrinthsOfTasylock
  , SomeLocationCard palaceGates
  , SomeLocationCard spiralStaircase
  , SomeLocationCard stalagmiteForest
  , SomeLocationCard surfaceOfFragment
  , SomeLocationCard theYellowThrone
  , SomeLocationCard brainStorage
  , SomeLocationCard communicator
  , SomeLocationCard dreamDiagnostics
  , SomeLocationCard memoryScanner
  , SomeLocationCard cityOfCats
  , SomeLocationCard feverDream
  , SomeLocationCard cargoHold
  , SomeLocationCard classroomK2
  , SomeLocationCard cryosleepQuarters
  , SomeLocationCard engineRoomTatterdemalion
  , SomeLocationCard coldWastes
  , SomeLocationCard crewQuarters
  , SomeLocationCard crystalPeak
  , SomeLocationCard engineRoomInTheShadowOfEarth
  , SomeLocationCard entranceHall
  , SomeLocationCard entranceTunnel
  , SomeLocationCard escapePodBay
  , SomeLocationCard flightDeck
  , SomeLocationCard gymnasium
  , SomeLocationCard infirmaryTatterdemalion
  , SomeLocationCard hydroponics
  , SomeLocationCard iceSpires
  , SomeLocationCard landingCraft
  , SomeLocationCard infirmaryInTheShadowOfEarth
  , SomeLocationCard library
  , SomeLocationCard mainFacility
  , SomeLocationCard messHall
  , SomeLocationCard omniTransmitters
  , SomeLocationCard qCrystalMines
  , SomeLocationCard realitySimulator
  , SomeLocationCard schoolGrounds
  , SomeLocationCard schrodGenerators
  , SomeLocationCard shipsBridge
  , SomeLocationCard ventilationShaft
  ]

stories :: [SomeStoryCard]
stories =
  [ SomeStoryCard evidenceAdamTanner
  , SomeStoryCard evidenceCaptainBurr
  , SomeStoryCard ritualOfTheSun
  , SomeStoryCard arrivalOfTheKing
  , SomeStoryCard delights
  , SomeStoryCard forYouAlone
  , SomeStoryCard lostExpedition
  , SomeStoryCard withoutATrace
  , SomeStoryCard theCultist
  , SomeStoryCard theMiner
  , SomeStoryCard theTeacher
  , SomeStoryCard evidenceDoctorFeng
  , SomeStoryCard evidenceLtArcherMichaels
  , SomeStoryCard evidenceMUD12Mudbug
  , SomeStoryCard evidenceSophie
  , SomeStoryCard finalDestination
  , SomeStoryCard intoTheArchives
  , SomeStoryCard k2PS187CyberneticBrains
  , SomeStoryCard reintegrated_062
  , SomeStoryCard reintegrated_063
  , SomeStoryCard reintegrated_064
  , SomeStoryCard reintegrated_065
  , SomeStoryCard strangeIsTheNight
  , SomeStoryCard whatTypeOfShipIsThis
  , SomeStoryCard whoAmI
  ]

treacheries :: [SomeTreacheryCard]
treacheries =
  [ SomeTreacheryCard allSeeingEye
  , SomeTreacheryCard anachronism
  , SomeTreacheryCard duplication
  , SomeTreacheryCard perfectImitation
  , SomeTreacheryCard entangled
  , SomeTreacheryCard incomprehensible
  , SomeTreacheryCard reminiscencePledge
  , SomeTreacheryCard reminiscenceSecrets
  , SomeTreacheryCard reminiscenceCovenant
  , SomeTreacheryCard echoesOfTassildaMatter
  , SomeTreacheryCard echoesOfTassildaMind
  , SomeTreacheryCard theDarkForest
  , SomeTreacheryCard fromTheDark
  , SomeTreacheryCard brokenReality
  , SomeTreacheryCard callOfTheVoid
  , SomeTreacheryCard alienation
  , SomeTreacheryCard alienAid
  , SomeTreacheryCard theColorsOfSpace
  , SomeTreacheryCard hastursDomain
  , SomeTreacheryCard innocentMishap
  , SomeTreacheryCard solarEclipse
  , SomeTreacheryCard unstableDimension
  , SomeTreacheryCard darkReflectionsMalingerer
  , SomeTreacheryCard darkReflectionsMurderer
  , SomeTreacheryCard darkReflectionsSycophant
  , SomeTreacheryCard darkReflectionsZealot
  , SomeTreacheryCard perspectiveSwitch
  , SomeTreacheryCard delusionalMadness
  , SomeTreacheryCard closeEncounters
  , SomeTreacheryCard extraterrestrialAssault
  , SomeTreacheryCard infection
  , SomeTreacheryCard irresistibleTruths
  , SomeTreacheryCard miGoExperiments
  , SomeTreacheryCard simulationDiscrepancy
  , SomeTreacheryCard toxicPits
  , SomeTreacheryCard anothersWoe
  , SomeTreacheryCard artificialGravityMalfunction
  , SomeTreacheryCard cabinPressure
  , SomeTreacheryCard caveCollapse
  , SomeTreacheryCard chosenByHim
  , SomeTreacheryCard coldVacuum
  , SomeTreacheryCard comeCLOSER
  , SomeTreacheryCard contamination
  , SomeTreacheryCard coolantLeak
  , SomeTreacheryCard decoherence
  , SomeTreacheryCard decompression
  , SomeTreacheryCard desync
  , SomeTreacheryCard digitalCorrosion
  , SomeTreacheryCard electricSurge
  , SomeTreacheryCard fathomlessRegrets
  , SomeTreacheryCard forbiddingPromises
  , SomeTreacheryCard futureEvils
  , SomeTreacheryCard grimFuture
  , SomeTreacheryCard hallucinatoryHolograms
  , SomeTreacheryCard hauntingPast
  , SomeTreacheryCard highRadiationLevels
  , SomeTreacheryCard hopeless
  , SomeTreacheryCard lostInTranslation
  , SomeTreacheryCard madnessOfCarcosa
  , SomeTreacheryCard micrometeoroid
  , SomeTreacheryCard nonEuclideanGeometry
  , SomeTreacheryCard paleBlueDot
  , SomeTreacheryCard paradoxicalThreat
  , SomeTreacheryCard persistenceOfMemory
  , SomeTreacheryCard predictiveAlgorithm
  , SomeTreacheryCard quantumCollapse
  , SomeTreacheryCard radiantCrown
  , SomeTreacheryCard radioactiveDecay
  , SomeTreacheryCard rememberME
  , SomeTreacheryCard sceneShifting
  , SomeTreacheryCard scrambled
  , SomeTreacheryCard solarFlare
  , SomeTreacheryCard songOfYourSoul
  , SomeTreacheryCard theStarsWereRight
  , SomeTreacheryCard surprise
  ]

scenarios :: [(CardCode, HomebrewScenario)]
scenarios =
  [ (":dark-matter:014", HomebrewScenario theTatterdemalion)
  , (":dark-matter:054", HomebrewScenario electricNightmare)
  , (":dark-matter:089", HomebrewScenario lostQuantum)
  , (":dark-matter:112", HomebrewScenario inTheShadowOfEarth)
  , (":dark-matter:153", HomebrewScenario strangeMoons)
  , (":dark-matter:190", HomebrewScenario theMachineInYellow)
  , (":dark-matter:209", HomebrewScenario fragmentOfCarcosa)
  , (":dark-matter:243", HomebrewScenario starfall)
  ]

scenarioSets :: [(CardCode, EncounterSet)]
scenarioSets =
  [ (":dark-matter:014", Sets.TheTatterdemalion)
  , (":dark-matter:054", Sets.ElectricNightmare)
  , (":dark-matter:089", Sets.LostQuantum)
  , (":dark-matter:112", Sets.InTheShadowOfEarth)
  , (":dark-matter:153", Sets.StrangeMoons)
  , (":dark-matter:190", Sets.TheMachineInYellow)
  , (":dark-matter:209", Sets.FragmentOfCarcosa)
  , (":dark-matter:243", Sets.Starfall)
  ]

campaigns :: [(CampaignId, HomebrewCampaign)]
campaigns = [(":dark-matter", HomebrewCampaign darkMatter)]

data DarkMatterContent

instance IsHomebrewContent DarkMatterContent where
  homebrewContent =
    HomebrewContent
      { acts = acts
      , agendas = agendas
      , assets = assets
      , enemies = enemies
      , locations = locations
      , stories = stories
      , treacheries = treacheries
      , scenarios = scenarios
      , scenarioSets = scenarioSets
      , campaigns = campaigns
      }
