module Arkham.EncounterSet where

import Arkham.Prelude

import Data.Char (isUpper)
import Data.Char qualified as Char
import Data.Data (dataTypeConstrs, dataTypeOf, fromConstr, showConstr)
import Data.Text qualified as T

data EncounterSet
  = TheGathering
  | TheMidnightMasks
  | TheDevourerBelow
  | CultOfUmordhoth
  | Rats
  | Ghouls
  | StrikingFear
  | AncientEvils
  | ChillingCold
  | Nightgaunts
  | DarkCult
  | LockedDoors
  | AgentsOfHastur
  | AgentsOfYogSothoth
  | AgentsOfShubNiggurath
  | AgentsOfCthulhu
  | ExtracurricularActivity
  | TheHouseAlwaysWins
  | ArmitagesFate
  | TheMiskatonicMuseum
  | TheEssexCountyExpress
  | BloodOnTheAltar
  | UndimensionedAndUnseen
  | WhereDoomAwaits
  | LostInTimeAndSpace
  | Sorcery
  | BishopsThralls
  | Dunwich
  | Whippoorwills
  | BadLuck
  | BeastThralls
  | NaomisCrew
  | TheBeyond
  | HideousAbominations
  | CurtainCall
  | TheLastKing
  | Delusions
  | Byakhee
  | InhabitantsOfCarcosa
  | EvilPortents
  | Hauntings
  | HastursGift
  | CultOfTheYellowSign
  | DecayAndFilth
  | TheStranger
  | EchoesOfThePast
  | TheUnspeakableOath
  | APhantomOfTruth
  | ThePallidMask
  | BlackStarsRise
  | DimCarcosa
  | TheUntamedWilds
  | TheDoomOfEztli
  | Rainforest
  | Serpents
  | Expedition
  | AgentsOfYig
  | GuardiansOfTime
  | DeadlyTraps
  | TemporalFlux
  | ForgottenRuins
  | PnakoticBrotherhood
  | YigsVenom
  | Poison
  | ThreadsOfFate
  | TheBoundaryBeyond
  | HeartOfTheElders
  | PillarsOfJudgement
  | KnYan
  | TheCityOfArchives
  | TheDepthsOfYoth
  | ShatteredAeons
  | TurnBackTime
  | DisappearanceAtTheTwilightEstate
  | TheWitchingHour
  | AtDeathsDoorstep
  | TheWatcher
  | AgentsOfAzathoth
  | AnettesCoven
  | Witchcraft
  | SilverTwilightLodge
  | CityOfSins
  | SpectralPredators
  | TrappedSpirits
  | RealmOfDeath
  | InexorableFate
  | TheSecretName
  | TheWagesOfSin
  | ForTheGreaterGood
  | UnionAndDisillusion
  | InTheClutchesOfChaos
  | MusicOfTheDamned
  | SecretsOfTheUniverse
  | BeforeTheBlackThrone
  | BeyondTheGatesOfSleep
  | WakingNightmare
  | AgentsOfAtlachNacha
  | AgentsOfNyarlathotep
  | WhispersOfHypnos
  | CreaturesOfTheUnderworld
  | DreamersCurse
  | Dreamlands
  | MergingRealities
  | Spiders
  | Corsairs
  | Zoogs
  | TheSearchForKadath
  | AThousandShapesOfHorror
  | DarkSideOfTheMoon
  | PointOfNoReturn
  | DescentIntoThePitch
  | TerrorOfTheVale
  | WhereTheGodsDwell
  | WeaverOfTheCosmos
  | ThePitOfDespair
  | TheVanishingOfElinaHarper
  | AgentsOfDagon
  | AgentsOfHydra
  | CreaturesOfTheDeep
  | RisingTide
  | FogOverInnsmouth
  | ShatteredMemories
  | Malfunction
  | Syzygy
  | FloodedCaverns
  | TheLocals
  | InTooDeep
  | DevilReef
  | HorrorInHighGear
  | ALightInTheFog
  | TheLairOfDagon
  | IntoTheMaelstrom
  | AgentsOfTheUnknown
  | CityOfTheElderThings
  | CreaturesInTheIce
  | DeadlyWeather
  | ElderThings
  | ExpeditionTeam
  | FatalMirage
  | HazardsOfAntarctica
  | IceAndDeath
  | LeftBehind
  | LostInTheNight
  | MemorialsOfTheLost
  | Miasma
  | NamelessHorrors
  | Penguins
  | SeepingNightmares
  | Shoggoths
  | SilenceAndMystery
  | StirringInTheDeep
  | Tekelili
  | TheCrash
  | TheGreatSeal
  | TheHeartOfMadness
  | ToTheForbiddenPeaks
  | RiddlesAndRain
  | DeadHeat
  | SanguineShadows
  | DealingsInTheDark
  | DancingMad
  | OnThinIce
  | DogsOfWar
  | ShadesOfSuffering
  | WithoutATrace
  | CongressOfTheKeys
  | CrimsonConspiracy
  | StrangeHappenings
  | MysteriesAbound
  | ShadowOfADoubt
  | DarkVeiling
  | CleanupCrew
  | ScarletSorcery
  | Outsiders
  | SecretWar
  | AgentsOfTheOutside
  | AgentsOfYuggoth
  | SpatialAnomaly
  | SpreadingCorruption
  | BeyondTheBeyond
  | RedCoterie
  | Globetrotting
  | WrittenInRock
  | HemlockHouse
  | TheSilentHeath
  | TheLostSister
  | TheThingInTheDepths
  | TheTwistedHollow
  | TheLongestNight
  | FateOfTheVale
  | TheFirstDay
  | TheSecondDay
  | TheFinalDay
  | DayOfRest
  | DayOfRain
  | DayOfTheFeast
  | Residents
  | TheVale
  | Heirlooms
  | HorrorsInTheRock
  | AgentsOfTheColour
  | Transfiguration
  | Blight
  | Refractions
  | TheForest
  | Myconids
  | Mutations
  | Fire
  | ReturnToTheGathering
  | ReturnToTheMidnightMasks
  | ReturnToTheDevourerBelow
  | GhoulsOfUmordhoth
  | TheDevourersCult
  | ReturnCultOfUmordhoth
  | ReturnToExtracurricularActivities
  | ReturnToTheHouseAlwaysWins
  | ReturnToTheMiskatonicMuseum
  | ReturnToTheEssexCountyExpress
  | ReturnToBloodOnTheAltar
  | ReturnToUndimensionedAndUnseen
  | ReturnToWhereDoomAwaits
  | ReturnToLostInTimeAndSpace
  | BeyondTheThreshold
  | ResurgentEvils
  | SecretDoors
  | CreepingCold
  | ErraticFear
  | YogSothothsEmissaries
  | ReturnToCurtainCall
  | ReturnToTheLastKing
  | ReturnToEchoesOfThePast
  | ReturnToTheUnspeakableOath
  | ReturnToAPhantomOfTruth
  | ReturnToThePallidMask
  | ReturnToBlackStarsRise
  | ReturnToDimCarcosa
  | DelusoryEvils
  | DecayingReality
  | HastursEnvoys
  | MaddeningDelusions
  | NeuroticFear
  | ReturnToTheUntamedWilds
  | ReturnToTheDoomOfEztli
  | ReturnToThreadsOfFate
  | ReturnToTheBoundaryBeyond
  | ReturnToHeartOfTheElders
  | ReturnToPillarsOfJudgement
  | ReturnToKnYan
  | ReturnToTheCityOfArchives
  | ReturnToTheDepthsOfYoth
  | ReturnToShatteredAeons
  | ReturnToTurnBackTime
  | ReturnToRainforest
  | CultOfPnakotus
  | DoomedExpedition
  | TemporalHunters
  | VenomousHate
  | ReturnToDisappearanceAtTheTwilightEstate
  | ReturnToTheWitchingHour
  | ReturnToAtDeathsDoorstep
  | ReturnToTheSecretName
  | ReturnToTheWagesOfSin
  | ReturnToForTheGreaterGood
  | ReturnToUnionAndDisillusion
  | ReturnToInTheClutchesOfChaos
  | ReturnToBeforeTheBlackThrone
  | Hexcraft
  | BloodthirstySpirits
  | UnspeakableFate
  | UnstableRealm
  | CityOfTheDamned
  | ChillingMists
  | ImpendingEvils
  | TheBayou
  | CurseOfTheRougarou
  | CarnevaleOfHorrors
  | TheLabyrinthsOfLunacy
  | LabyrinthsOfLunacySingleGroup
  | LabyrinthsOfLunacyEpicMultiplayer
  | TheEternalSlumber
  | TheNightsUsurper
  | BrotherhoodOfTheBeast
  | SandsOfEgypt
  | AbyssalTribute
  | AbyssalGifts
  | WarOfTheOuterGods
  | DeathOfStars
  | ChildrenOfParadise
  | SwarmOfAssimilation
  | MurderAtTheExcelsiorHotel
  | AlienInterference
  | ExcelsiorManagement
  | DarkRituals
  | VileExperiments
  | SinsOfThePast
  | TheBlobThatAteEverythingELSE
  | TheBlobThatAteEverything
  | MiGoIncursion
  | BlobEpicMultiplayer
  | BlobSingleGroup
  | FortuneAndFolly
  | FortunesChosen
  | PlanInShambles
  | TheMidwinterGala
  | MachinationsThroughTime
  | MachinationsThroughTimeSingleGroup
  | MachinationsThroughTimeEpicMultiplayer
  | FilmFatale
  | CosmicJourney
  | ForgottenIsland
  | AbominableContessa
  | SpreadingFlames
  | AshenPilgrims
  | Bystanders
  | CosmicEvils
  | EldritchLore
  | Hallucinations
  | Fire1
  | MadScience
  | ArcaneLock
  | BadWeather
  | DeadEnds
  | Torment
  | GangsOfArkham
  | Cultists
  | ReekingDecay
  | FlyingTerrors
  | MiskatonicUniversity
  | Arkham
  | PeopleOfArkham
  | Whippoorwills2
  | Sewers
  | SmokeAndMirrors
  | QueenOfAsh
  | ReadOrDie
  | AllOrNothing
  | BadBlood
  | ByTheBook
  | RedTideRising
  | LaidToRest
  | RelicsOfThePast
  | EnthrallingEncore
  | -- The Drowned City
    OneLastJob
  | TheWesternWall
  | TheDrownedQuarter
  | TheApiary
  | TheGrandVault
  | CourtOfTheAncients
  | ObsidianCanyons
  | SepulchreOfTheSleeper
  | TheDoomOfArkhamPartI
  | TheDoomOfArkhamPartII
  | Tasks
  | TdcExpedition
  | StarSpawn
  | UnderseaCreatures
  | Flood
  | Domination
  | DeepOnes
  | Stowaways
  | Pilgrims
  | CosmicLegacy
  | ElderMist
  | Rlyeh
  | TheInescapable
  | Dreams
  | AlienMachinery
  | Homebrew Text
  | Test
  deriving stock (Show, Eq, Ord, Data)

-- | JSON encoding is kept compatible with the original all-nullary string form:
-- official sets encode as their constructor name, homebrew sets as their slug
-- (e.g. @":dark-matter:anachronism"@). Parsing falls back to 'Homebrew' for any
-- unrecognized string, and remaps legacy campaign-prefixed constructor names
-- (from when homebrew sets were real constructors) onto their slugs.
instance ToJSON EncounterSet where
  toJSON (Homebrew t) = String t
  toJSON s = String (tshow s)

officialEncounterSets :: Map Text EncounterSet
officialEncounterSets =
  mapFromList
    [ (pack name, fromConstr c)
    | c <- dataTypeConstrs (dataTypeOf (Test :: EncounterSet))
    , let name = showConstr c
    , name /= "Homebrew"
    ]

instance FromJSON EncounterSet where
  parseJSON = withText "EncounterSet" \t ->
    pure $ fromMaybe (Homebrew (legacySlug t)) (lookup t officialEncounterSets)

-- | Remap legacy homebrew constructor names ("DarkMatterAnachronism") to slugs
-- (":dark-matter:anachronism"); anything else is assumed to already be a slug.
legacySlug :: Text -> Text
legacySlug t
  | Just rest <- T.stripPrefix "DarkMatter" t, not (T.null rest) = ":dark-matter:" <> toSnake rest
  | Just rest <- T.stripPrefix "CircusExMortis" t, not (T.null rest) = ":circus-ex-mortis:" <> toSnake rest
  | otherwise = t
 where
  toSnake = T.dropWhile (== '_') . T.concatMap \c -> if isUpper c then T.pack ['_', Char.toLower c] else T.singleton c

