module Arkham.Homebrew.DarkMatter.Content where

import Arkham.Act.Types (SomeActCard (..))
import Arkham.Agenda.Types (SomeAgendaCard (..))
import Arkham.Asset.Types (SomeAssetCard (..))
import Arkham.Card.CardCode
import Arkham.EncounterSet (EncounterSet)
import Arkham.Enemy.Types (SomeEnemyCard (..))
import Arkham.Homebrew.DarkMatter.Acts.ArtificalInsanity
import Arkham.Homebrew.DarkMatter.Acts.EventHorizon
import Arkham.Homebrew.DarkMatter.Acts.FacingYourFears
import Arkham.Homebrew.DarkMatter.Acts.Psychoanalysis
import Arkham.Homebrew.DarkMatter.Acts.PublicSchool187
import Arkham.Homebrew.DarkMatter.Acts.Reconnected
import Arkham.Homebrew.DarkMatter.Agendas.EmergencyProcedure
import Arkham.Homebrew.DarkMatter.Agendas.FigmentOfYourImagination
import Arkham.Homebrew.DarkMatter.Agendas.It
import Arkham.Homebrew.DarkMatter.Agendas.RiseOfTheMachines
import Arkham.Homebrew.DarkMatter.Agendas.TheGhostShip
import Arkham.Homebrew.DarkMatter.Assets.EvaSuit
import Arkham.Homebrew.DarkMatter.Assets.HeirToCarcosa
import Arkham.Homebrew.DarkMatter.Assets.K2PS187100Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18725Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18750Functionality
import Arkham.Homebrew.DarkMatter.Assets.K2PS18775Functionality
import Arkham.Homebrew.DarkMatter.Assets.Maja
import Arkham.Homebrew.DarkMatter.Assets.MedicalFoam
import Arkham.Homebrew.DarkMatter.Assets.MindMachineInterface
import Arkham.Homebrew.DarkMatter.Assets.RadiationTablets
import Arkham.Homebrew.DarkMatter.Assets.VirtualAccessKey
import Arkham.Homebrew.DarkMatter.Campaign (darkMatter)
import Arkham.Homebrew.DarkMatter.Enemies.Cybervirus
import Arkham.Homebrew.DarkMatter.Enemies.GlitchInTheSystem
import Arkham.Homebrew.DarkMatter.Enemies.Jv7Hyades
import Arkham.Homebrew.DarkMatter.Enemies.Lr02Hali
import Arkham.Homebrew.DarkMatter.Enemies.ManifestedWhispers
import Arkham.Homebrew.DarkMatter.Enemies.ShadowOfThoughts
import Arkham.Homebrew.DarkMatter.Enemies.StalkingByakhee
import Arkham.Homebrew.DarkMatter.Enemies.SystemBug
import Arkham.Homebrew.DarkMatter.Enemies.TheBOOGEYMAN
import Arkham.Homebrew.DarkMatter.Enemies.TheFeasterFromAfar
import Arkham.Homebrew.DarkMatter.Enemies.UplA21Demhe
import Arkham.Homebrew.DarkMatter.Enemies.ViciousByakhee
import Arkham.Homebrew.DarkMatter.Enemies.VirtualByakhee
import Arkham.Homebrew.DarkMatter.Locations.BiologyLab
import Arkham.Homebrew.DarkMatter.Locations.Cafeteria
import Arkham.Homebrew.DarkMatter.Locations.CargoHold
import Arkham.Homebrew.DarkMatter.Locations.ClassroomK2
import Arkham.Homebrew.DarkMatter.Locations.CryosleepQuarters
import Arkham.Homebrew.DarkMatter.Locations.EngineRoomTatterdemalion
import Arkham.Homebrew.DarkMatter.Locations.EntranceHall
import Arkham.Homebrew.DarkMatter.Locations.EscapePodBay
import Arkham.Homebrew.DarkMatter.Locations.Gymnasium
import Arkham.Homebrew.DarkMatter.Locations.InfirmaryTatterdemalion
import Arkham.Homebrew.DarkMatter.Locations.Library
import Arkham.Homebrew.DarkMatter.Locations.MessHall
import Arkham.Homebrew.DarkMatter.Locations.SchoolGrounds
import Arkham.Homebrew.DarkMatter.Locations.ShipsBridge
import Arkham.Homebrew.DarkMatter.Locations.VentilationShaft
import Arkham.Homebrew.DarkMatter.Scenarios.ElectricNightmare
import Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa
import Arkham.Homebrew.DarkMatter.Scenarios.InTheShadowOfEarth
import Arkham.Homebrew.DarkMatter.Scenarios.LostQuantum
import Arkham.Homebrew.DarkMatter.Scenarios.Starfall
import Arkham.Homebrew.DarkMatter.Scenarios.StrangeMoons
import Arkham.Homebrew.DarkMatter.Scenarios.TheMachineInYellow
import Arkham.Homebrew.DarkMatter.Scenarios.TheTatterdemalion
import Arkham.Homebrew.DarkMatter.Sets qualified as Sets
import Arkham.Homebrew.DarkMatter.Stories.FinalDestination
import Arkham.Homebrew.DarkMatter.Stories.IntoTheArchives
import Arkham.Homebrew.DarkMatter.Stories.K2PS187CyberneticBrains
import Arkham.Homebrew.DarkMatter.Stories.Reintegrated
import Arkham.Homebrew.DarkMatter.Stories.StrangeIsTheNight
import Arkham.Homebrew.DarkMatter.Stories.WhatTypeOfShipIsThis
import Arkham.Homebrew.DarkMatter.Stories.WhoAmI
import Arkham.Homebrew.DarkMatter.Treacheries.AllSeeingEye
import Arkham.Homebrew.DarkMatter.Treacheries.Anachronism
import Arkham.Homebrew.DarkMatter.Treacheries.AnothersWoe
import Arkham.Homebrew.DarkMatter.Treacheries.ArtificialGravityMalfunction
import Arkham.Homebrew.DarkMatter.Treacheries.CabinPressure
import Arkham.Homebrew.DarkMatter.Treacheries.CaveCollapse
import Arkham.Homebrew.DarkMatter.Treacheries.ChosenByHim
import Arkham.Homebrew.DarkMatter.Treacheries.ColdVacuum
import Arkham.Homebrew.DarkMatter.Treacheries.ComeCLOSER
import Arkham.Homebrew.DarkMatter.Treacheries.Contamination
import Arkham.Homebrew.DarkMatter.Treacheries.CoolantLeak
import Arkham.Homebrew.DarkMatter.Treacheries.Decoherence
import Arkham.Homebrew.DarkMatter.Treacheries.Decompression
import Arkham.Homebrew.DarkMatter.Treacheries.Desync
import Arkham.Homebrew.DarkMatter.Treacheries.DigitalCorrosion
import Arkham.Homebrew.DarkMatter.Treacheries.ElectricSurge
import Arkham.Homebrew.DarkMatter.Treacheries.FathomlessRegrets
import Arkham.Homebrew.DarkMatter.Treacheries.ForbiddingPromises
import Arkham.Homebrew.DarkMatter.Treacheries.FutureEvils
import Arkham.Homebrew.DarkMatter.Treacheries.GrimFuture
import Arkham.Homebrew.DarkMatter.Treacheries.HallucinatoryHolograms
import Arkham.Homebrew.DarkMatter.Treacheries.HauntingPast
import Arkham.Homebrew.DarkMatter.Treacheries.HighRadiationLevels
import Arkham.Homebrew.DarkMatter.Treacheries.Hopeless
import Arkham.Homebrew.DarkMatter.Treacheries.LostInTranslation
import Arkham.Homebrew.DarkMatter.Treacheries.MadnessOfCarcosa
import Arkham.Homebrew.DarkMatter.Treacheries.Micrometeoroid
import Arkham.Homebrew.DarkMatter.Treacheries.NonEuclideanGeometry
import Arkham.Homebrew.DarkMatter.Treacheries.PaleBlueDot
import Arkham.Homebrew.DarkMatter.Treacheries.PersistenceOfMemory
import Arkham.Homebrew.DarkMatter.Treacheries.PredictiveAlgorithm
import Arkham.Homebrew.DarkMatter.Treacheries.RadiantCrown
import Arkham.Homebrew.DarkMatter.Treacheries.RememberME
import Arkham.Homebrew.DarkMatter.Treacheries.SceneShifting
import Arkham.Homebrew.DarkMatter.Treacheries.Scrambled
import Arkham.Homebrew.DarkMatter.Treacheries.SolarFlare
import Arkham.Homebrew.DarkMatter.Treacheries.SongOfYourSoul
import Arkham.Homebrew.DarkMatter.Treacheries.TheStarsWereRight
import Arkham.Homebrew.DarkMatter.Treacheries.Surprise
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
  , SomeActCard psychoanalysis
  , SomeActCard facingYourFears
  ]

agendas :: [SomeAgendaCard]
agendas =
  [ SomeAgendaCard emergencyProcedure
  , SomeAgendaCard theGhostShip
  , SomeAgendaCard riseOfTheMachines
  , SomeAgendaCard figmentOfYourImagination
  , SomeAgendaCard it
  ]

assets :: [SomeAssetCard]
assets =
  [ SomeAssetCard virtualAccessKey
  , SomeAssetCard evaSuit
  , SomeAssetCard heirToCarcosa
  , SomeAssetCard medicalFoam
  , SomeAssetCard mindMachineInterface
  , SomeAssetCard radiationTablets
  , SomeAssetCard maja
  , SomeAssetCard k2PS187100Functionality
  , SomeAssetCard k2PS18725Functionality
  , SomeAssetCard k2PS18750Functionality
  , SomeAssetCard k2PS18775Functionality
  ]

enemies :: [SomeEnemyCard]
enemies =
  [ SomeEnemyCard cybervirus
  , SomeEnemyCard glitchInTheSystem
  , SomeEnemyCard jv7Hyades
  , SomeEnemyCard lr02Hali
  , SomeEnemyCard manifestedWhispers
  , SomeEnemyCard shadowOfThoughts
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
  [ SomeLocationCard biologyLab
  , SomeLocationCard cafeteria
  , SomeLocationCard cargoHold
  , SomeLocationCard classroomK2
  , SomeLocationCard cryosleepQuarters
  , SomeLocationCard engineRoomTatterdemalion
  , SomeLocationCard entranceHall
  , SomeLocationCard escapePodBay
  , SomeLocationCard gymnasium
  , SomeLocationCard infirmaryTatterdemalion
  , SomeLocationCard library
  , SomeLocationCard messHall
  , SomeLocationCard schoolGrounds
  , SomeLocationCard shipsBridge
  , SomeLocationCard ventilationShaft
  ]

stories :: [SomeStoryCard]
stories =
  [ SomeStoryCard finalDestination
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
  , SomeTreacheryCard persistenceOfMemory
  , SomeTreacheryCard predictiveAlgorithm
  , SomeTreacheryCard radiantCrown
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
