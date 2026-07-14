module Arkham.Homebrew.CircusExMortis.Defs (module Arkham.Homebrew.CircusExMortis.Defs) where

import Arkham.Card.CardDef
import Arkham.Homebrew.DefsBase
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Skills qualified as Skills
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.CircusExMortis.Traits qualified as Traits

locations :: [CardDef]
locations =
  [ Locations.animalCages
  , Locations.banquetHall
  , Locations.boxcar
  , Locations.caboose
  , Locations.campOutskirtsGuardedClosely
  , Locations.campOutskirtsQuietForNow
  , Locations.canyonEntrance
  , Locations.carousel
  , Locations.circusEncampment
  , Locations.circusEngine
  , Locations.circusGatesDoorwayToDoom
  , Locations.circusGatesPathToFreedom
  , Locations.coalHopperCar
  , Locations.collectionHall
  , Locations.craneCar
  , Locations.crowdedRow_048
  , Locations.crowdedRow_049
  , Locations.crowdedRow_050
  , Locations.crowdedRow_051
  , Locations.defiledWoods
  , Locations.exoticAnimalCar
  , Locations.fallenCopse
  , Locations.flatcar
  , Locations.foothillSlope_162
  , Locations.foothillSlope_163
  , Locations.foothillSlope_164
  , Locations.foothillSlope_165
  , Locations.forestChasm
  , Locations.forestPassage
  , Locations.forgottenTrail
  , Locations.gamesGallery
  , Locations.gondolaCar
  , Locations.hiddenDungeon
  , Locations.highThicket
  , Locations.locomotiveEngine
  , Locations.mailCar
  , Locations.manorCellars
  , Locations.markedGrove
  , Locations.moonlitForestCircularGrove
  , Locations.moonlitForestDeadGrove
  , Locations.moonlitForestFogBank
  , Locations.moonlitForestGlassyLake
  , Locations.moonlitForestLabyrinthOfTrees
  , Locations.moonlitForestMistyMarsh
  , Locations.moonlitForestQuietValley
  , Locations.moonlitForestShadowedPath
  , Locations.moonlitForestShallowRiver
  , Locations.moonlitForestSmolderingCampfire
  , Locations.mossyGlen
  , Locations.mountainStream_166
  , Locations.mountainStream_167
  , Locations.mountainStream_168
  , Locations.mountainStream_169
  , Locations.openForest_170
  , Locations.openForest_171
  , Locations.openForest_172
  , Locations.performerTrailers
  , Locations.performersCar
  , Locations.primalForest
  , Locations.privateParlor
  , Locations.refrigeratorCar
  , Locations.reinforcedCar
  , Locations.remoteCabin
  , Locations.ringmastersTrailer
  , Locations.ritualClearing
  , Locations.savageAltar
  , Locations.secludedTent_052
  , Locations.secludedTent_053
  , Locations.secludedTent_054
  , Locations.secludedTent_055
  , Locations.shadowedWilderness_173
  , Locations.shadowedWilderness_174
  , Locations.shadowedWilderness_175
  , Locations.shadowedWilderness_176
  , Locations.shadowedWilderness_177
  , Locations.silentClearing
  , Locations.sparseWoodland
  , Locations.statuaryGardens
  , Locations.stockCar
  , Locations.tankCar
  , Locations.theBigTopFirstRing
  , Locations.theBigTopSecondRing
  , Locations.theBigTopThirdRing
  , Locations.upperBalcony
  , Locations.vestibule
  , Locations.woodlandOverlook
  ]

enemies :: [CardDef]
enemies =
  [ Enemies.brashLothario
  , Enemies.circusPredator
  , Enemies.darkYoungJuggernaut
  , Enemies.devoteeOfTheThousand
  , Enemies.disguisedMonstrosity
  , Enemies.goatspawnCorruptor
  , Enemies.grotesqueLion
  , Enemies.loomingGoatspawn
  , Enemies.maliciousGoatspawn
  , Enemies.mooncalf
  , Enemies.nascentDarkYoung
  , Enemies.newMoonAcrobat
  , Enemies.newMoonBeastTamer
  , Enemies.newMoonCarny
  , Enemies.newMoonClown
  , Enemies.newMoonDrudge
  , Enemies.newMoonIllusionist
  , Enemies.newMoonMagician
  , Enemies.newMoonStiltwalker
  , Enemies.newMoonStrongman
  , Enemies.newMoonTumbler
  , Enemies.partyAnimal
  , Enemies.rampagingGoatspawn
  , Enemies.ravenousBrood
  , Enemies.ravenousGoatspawn
  , Enemies.roamingDarkYoung
  , Enemies.sacrificialShepherd
  , Enemies.sadisticSocialite
  , Enemies.shubNiggurath
  , Enemies.struttingPeacock
  , Enemies.supplicantOfTheGoat
  , Enemies.sylvesterBlake
  , Enemies.theCultEnMasseBlackGoatsRapture
  , Enemies.theCultEnMasseLeaderlessFanaticism
  , Enemies.theCultEnMasseRingmastersFervor
  , Enemies.toweringDarkYoung_067
  , Enemies.toweringDarkYoung_068
  , Enemies.toweringDarkYoung_069
  , Enemies.toweringDarkYoung_070
  , Enemies.toweringDarkYoung_071
  , Enemies.twistedSatyr
  , Enemies.ursineBrute
  , Enemies.writhingGoatspawn
  ]

treacheries :: [CardDef]
treacheries =
  [ Treacheries.allThatGlitters
  , Treacheries.balefulEclipse
  , Treacheries.bestLeftUnsaid
  , Treacheries.brokenCouplings
  , Treacheries.closeWatch
  , Treacheries.crashingTrees
  , Treacheries.denseTangle
  , Treacheries.destructiveImpulses
  , Treacheries.dreadOfTheNewMoon
  , Treacheries.drinkAndBeMerry
  , Treacheries.duplicitousIllusion
  , Treacheries.endlessSpawn
  , Treacheries.feralImpulses
  , Treacheries.focusedSabotage
  , Treacheries.hungerOfThousands
  , Treacheries.hypnoticGlamour
  , Treacheries.ireOfShubNiggurath
  , Treacheries.keepQuiet
  , Treacheries.lostAllControl
  , Treacheries.lostTheTrail
  , Treacheries.lunarInfluence
  , Treacheries.maddeningSpectacle
  , Treacheries.milkOfShubNiggurath
  , Treacheries.moonlightIllusion
  , Treacheries.ominousMoonlight
  , Treacheries.overloadedEngine
  , Treacheries.perfumeAndPassion
  , Treacheries.phantasmalDeception
  , Treacheries.phantomBeasts
  , Treacheries.primordialEvils
  , Treacheries.quickerThanTheEye
  , Treacheries.recklessStunt
  , Treacheries.ricketyRide
  , Treacheries.shadowyPerformance
  , Treacheries.silentForest
  , Treacheries.violentThrashing
  , Treacheries.wildHysteria
  ]

playerTreacheries :: [CardDef]
playerTreacheries = []

acts :: [CardDef]
acts =
  [ Acts.ageOldVisions
  , Acts.allsFair
  , Acts.audienceParticipationVI
  , Acts.audienceParticipationVII
  , Acts.audienceParticipationVIII
  , Acts.behindClosedDoors
  , Acts.deeperProfanities
  , Acts.engineTrouble
  , Acts.escapeActVI
  , Acts.escapeActVII
  , Acts.fashionablyEarly
  , Acts.forestOfGiantsVI
  , Acts.forestOfGiantsVII
  , Acts.forestOfGiantsVIII
  , Acts.forestOfIllusion
  , Acts.impendingZenith
  , Acts.noFreeRides
  , Acts.outAndAway
  , Acts.overdueDeparture
  , Acts.ratsInACage_005
  , Acts.ratsInACage_006
  , Acts.ratsInACage_007
  , Acts.ratsInACage_008
  , Acts.smokeAndMirrors
  , Acts.theGreatTrainHorror
  , Acts.theTrueMonster
  , Acts.throughTheForestsVI
  , Acts.throughTheForestsVII
  ]

agendas :: [CardDef]
agendas =
  [ Agendas.bloodMoon
  , Agendas.doomAndGloom
  , Agendas.fadingSunlightVI
  , Agendas.fadingSunlightVII
  , Agendas.feverPitch
  , Agendas.houseOfHorrors
  , Agendas.intoTheLionsDen
  , Agendas.lackOfRestraint
  , Agendas.mesmericMagic
  , Agendas.repeatShowing
  , Agendas.savageNature
  , Agendas.scheduleToKeep
  , Agendas.sleepWhenYoureDead
  , Agendas.theCircusSleeps
  , Agendas.theTrueFace
  , Agendas.treadingOnEggshells
  , Agendas.underMoonlessSkies
  , Agendas.whirlingSpectacle
  ]

encounterAssets :: [CardDef]
encounterAssets =
  [ Assets.amaltheaWeaverAspirantOfCourage
  , Assets.amaltheaWeaverAspirantOfWisdom
  , Assets.amaltheaWeaverCircusFortuneTeller
  , Assets.amaltheaWeaverOracleOfEnlightenment
  , Assets.amaltheaWeaverOracleOfMystery
  , Assets.amaltheaWeaverOracleOfPurity
  , Assets.amaltheaWeaverOracleOfResolve
  , Assets.carrieDykstra
  , Assets.cecilSharpe
  , Assets.deCultusBestiaeForgottenWorkOfApuleius
  , Assets.deCultusBestiaeInterpretationOfConviction
  , Assets.deCultusBestiaeInterpretationOfObsession
  , Assets.deCultusBestiaeProphecyOfTheBehemoth
  , Assets.deCultusBestiaeProphecyOfTheBeyond
  , Assets.deCultusBestiaeProphecyOfTheEternal
  , Assets.deCultusBestiaeProphecyOfTheHorde
  , Assets.estherMeredith
  , Assets.illusoryLocus
  , Assets.phillipHutchins
  , Assets.ralphDykstra
  , Assets.richardStratton
  , Assets.terrifiedCaptives
  , Assets.veraAshcroft
  ]

playerSkills :: [CardDef]
playerSkills =
  [ Skills.invocationOfDiana
  ]

stories :: [CardDef]
stories =
  [ Stories.bearTheBurden
  , Stories.cautiousJailers
  , Stories.clappedInIrons
  , Stories.cleanseTheStain
  , Stories.deepInTheDark
  , Stories.hiddenInPlainSight
  , Stories.hypnoticState
  , Stories.pathForward_180
  , Stories.pathForward_181
  , Stories.pathForward_182
  , Stories.pathForward_183
  , Stories.raiseTheTorch
  , Stories.reciteThePrayer
  , Stories.scribeTheSigil
  , Stories.silenceThePipes
  , Stories.splitTheRock
  , Stories.strikeTheHeart
  , Stories.theDarkYoungStir
  , Stories.underLockAndKey
  ]

data CircusExMortisDefs

instance IsHomebrewDefs CircusExMortisDefs where
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
      , hdActions = []
      , hdActionAffordability = []
      }
