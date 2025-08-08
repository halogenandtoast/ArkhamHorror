{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Enemy where

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Enemies
import Arkham.Enemy.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

createEnemy :: (HasCallStack, IsCard a) => a -> EnemyId -> Enemy
createEnemy a eid = lookupEnemy (toCardCode a) eid (toCardId a)

instance RunMessage Enemy where
  runMessage (SendMessage target msg) e | e `is` target = do
    runMessage msg e
  runMessage msg e@(Enemy x) = do
    -- we must check that an enemy exists when grabbing modifiers
    -- as some messages are not masked when targetting cards in the
    -- discard.
    allEnemyIds <- select AnyEnemy
    modifiers' <-
      if toId e `elem` allEnemyIds
        then getModifiers (toTarget e)
        else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Enemy <$> runMessage msg' x

lookupEnemy :: HasCallStack => CardCode -> EnemyId -> CardId -> Enemy
lookupEnemy cardCode = case lookup cardCode allEnemies of
  Nothing -> error $ "Unknown enemy: " <> show cardCode
  Just (SomeEnemyCard a) -> \e c -> Enemy $ cbCardBuilder a c e

instance FromJSON Enemy where
  parseJSON = withObject "Enemy" $ \o -> do
    cCode <- o .: "cardCode"
    withEnemyCardCode cCode
      $ \(_ :: EnemyCard a) -> Enemy <$> parseJSON @a (Object o)

withEnemyCardCode
  :: CardCode -> (forall a. IsEnemy a => EnemyCard a -> r) -> r
withEnemyCardCode cCode f = case lookup cCode allEnemies of
  Nothing -> error $ "Unknown enemy: " <> show cCode
  Just (SomeEnemyCard a) -> f a

allEnemies :: Map CardCode SomeEnemyCard
allEnemies =
  mapFrom
    someEnemyCardCode
    [ -- Night of the Zealot
      -- weakness
      SomeEnemyCard mobEnforcer
    , SomeEnemyCard silverTwilightAcolyte
    , SomeEnemyCard stubbornDetective
    , -- The Gathering
      SomeEnemyCard ghoulPriest
    , SomeEnemyCard fleshEater
    , SomeEnemyCard icyGhoul
    , -- The Midnight Masks
      SomeEnemyCard theMaskedHunter
    , SomeEnemyCard wolfManDrew
    , SomeEnemyCard hermanCollins
    , SomeEnemyCard peterWarren
    , SomeEnemyCard victoriaDevereux
    , SomeEnemyCard ruthTurner
    , -- The Devourer Below
      SomeEnemyCard umordhoth
    , -- Rats
      SomeEnemyCard swarmOfRats
    , -- Ghouls
      SomeEnemyCard ghoulMinion
    , SomeEnemyCard ravenousGhoul
    , -- Dark Cult
      SomeEnemyCard acolyte
    , SomeEnemyCard wizardOfTheOrder
    , -- Nightgaunts
      SomeEnemyCard huntingNightgaunt
    , -- Agents of Hastur
      SomeEnemyCard screechingByakhee
    , -- Agents of Yog-Sothoth
      SomeEnemyCard yithianObserver
    , -- Agents of Shub-Niggurath
      SomeEnemyCard relentlessDarkYoung
    , SomeEnemyCard goatSpawn
    , -- Agents of Cthulhu
      SomeEnemyCard youngDeepOne
    , -- The Dunwich Legacy
      -- Extracurricular Activity
      SomeEnemyCard theExperiment
    , -- The House Always Wins
      SomeEnemyCard cloverClubPitBoss
    , -- Bishop's Thralls
      SomeEnemyCard thrall
    , SomeEnemyCard wizardOfYogSothoth
    , -- Whippoorwill
      SomeEnemyCard whippoorwill
    , -- Beast Thralls
      SomeEnemyCard avianThrall
    , SomeEnemyCard lupineThrall
    , -- Naomi's Crew
      SomeEnemyCard oBannionsThug
    , SomeEnemyCard mobster
    , -- Hideous Abominations
      SomeEnemyCard conglomerationOfSpheres
    , SomeEnemyCard servantOfTheLurker
    , -- The Miskatonic Museum
      SomeEnemyCard huntingHorror
    , -- The Essex County Express
      SomeEnemyCard grapplingHorror
    , SomeEnemyCard emergentMonstrosity
    , -- Blood on the Altar
      SomeEnemyCard silasBishop
    , SomeEnemyCard servantOfManyMouths
    , -- Undimensioned and Unseen
      SomeEnemyCard broodOfYogSothoth
    , -- Where Doom Awaits
      SomeEnemyCard sethBishop
    , SomeEnemyCard devoteeOfTheKey
    , SomeEnemyCard crazedShoggoth
    , -- Lost in Time and Space
      SomeEnemyCard yogSothoth
    , SomeEnemyCard interstellarTraveler
    , SomeEnemyCard yithianStarseeker
    , -- The Path to Carcosa
      -- signature
      SomeEnemyCard graveyardGhouls
    , -- weakness
      SomeEnemyCard theThingThatFollows
    , -- Curtain Call
      SomeEnemyCard theManInThePallidMask
    , SomeEnemyCard royalEmissary
    , -- The Last King
      SomeEnemyCard constanceDumaine
    , SomeEnemyCard jordanPerry
    , SomeEnemyCard ishimaruHaruko
    , SomeEnemyCard sebastienMoreau
    , SomeEnemyCard ashleighClarke
    , SomeEnemyCard dianneDevine
    , -- Byakhee
      SomeEnemyCard swiftByakhee
    , -- Inhabitants of Carcosa
      SomeEnemyCard beastOfAldebaran
    , SomeEnemyCard spawnOfHali
    , -- Hauntings
      SomeEnemyCard poltergeist
    , -- Hastur's Gift
      SomeEnemyCard maniac
    , SomeEnemyCard youngPsychopath
    , -- Cult of the Yellow Sign
      SomeEnemyCard fanatic
    , SomeEnemyCard agentOfTheKing
    , -- Decay and Filth
      SomeEnemyCard roachSwarm
    , -- Echoes of the Past
      SomeEnemyCard possessedOathspeaker
    , SomeEnemyCard seekerOfCarcosa
    , -- The Unspeakable Oath
      SomeEnemyCard danielChesterfield
    , SomeEnemyCard asylumGorger
    , SomeEnemyCard madPatient
    , -- A Phantom of Truth
      SomeEnemyCard theOrganistHopelessIDefiedHim
    , SomeEnemyCard theOrganistDrapedInMystery
    , SomeEnemyCard stealthyByakhee
    , -- The Pallid Mask
      SomeEnemyCard specterOfDeath
    , SomeEnemyCard catacombsDocent
    , SomeEnemyCard corpseDweller
    , -- Black Stars Rise
      SomeEnemyCard tidalTerror
    , SomeEnemyCard riftSeeker
    , -- Dim Carcosa
      SomeEnemyCard hasturTheKingInYellow
    , SomeEnemyCard hasturLordOfCarcosa
    , SomeEnemyCard hasturTheTatteredKing
    , SomeEnemyCard creatureOutOfDemhe
    , SomeEnemyCard wingedOne
    , -- The Forgotten Age
      -- signature
      SomeEnemyCard serpentsOfYig
    , -- The Untamed Wilds
      SomeEnemyCard ichtaca
    , -- The Doom of Eztli
      SomeEnemyCard harbingerOfValusia
    , -- Serpents
      SomeEnemyCard pitViper
    , SomeEnemyCard boaConstrictor
    , -- Agents of Yig
      SomeEnemyCard broodOfYig
    , SomeEnemyCard serpentFromYoth
    , -- Guardians of Time
      SomeEnemyCard eztliGuardian
    , -- Pnakotic Brotherhood
      SomeEnemyCard brotherhoodCultist
    , -- Yig's Venom
      SomeEnemyCard fangOfYig
    , -- Threads of Fate
      SomeEnemyCard harlanEarnstoneCrazedByTheCurse
    , SomeEnemyCard henryDeveauAlejandrosKidnapper
    , SomeEnemyCard mariaDeSilvaKnowsMoreThanSheLetsOn
    , -- The Boundary Beyond
      SomeEnemyCard padmaAmrita
    , SomeEnemyCard serpentOfTenochtitlan
    , SomeEnemyCard handOfTheBrotherhood
    , -- Heart of the Elders
      --- Pillars of Judgement
      SomeEnemyCard theWingedSerpent
    , SomeEnemyCard apexStrangleweed
    , SomeEnemyCard basilisk
    , -- The City of Archives
      SomeEnemyCard keeperOfTheGreatLibrary
    , SomeEnemyCard scientistOfYith
    , SomeEnemyCard scholarFromYith
    , -- The Depths of Yoth
      SomeEnemyCard yig
    , SomeEnemyCard pitWarden
    , SomeEnemyCard eaterOfTheDepths
    , -- Shattered Aeons
      SomeEnemyCard ichtacaScionOfYig
    , SomeEnemyCard alejandroVela
    , SomeEnemyCard formlessSpawn
    , SomeEnemyCard temporalDevourer
    , SomeEnemyCard flyingPolyp
    , -- The Circle Undone
      -- signature
      SomeEnemyCard hoods
    , -- The Witching Hour
      SomeEnemyCard anetteMason
    , -- At Death's Doorstep
      SomeEnemyCard josefMeiger
    , -- The Watcher
      SomeEnemyCard theSpectralWatcher
    , -- Agents of Azathoth
      SomeEnemyCard piperOfAzathoth
    , -- Anette's Coven
      SomeEnemyCard covenInitiate
    , SomeEnemyCard priestessOfTheCoven
    , -- Silver Twilight Lodge
      SomeEnemyCard lodgeNeophyte
    , SomeEnemyCard keeperOfSecrets
    , -- Spectral Predators
      SomeEnemyCard netherMist
    , SomeEnemyCard shadowHound
    , -- Trapped Spirits
      SomeEnemyCard wraith
    , -- The Secret Name
      SomeEnemyCard brownJenkin
    , SomeEnemyCard nahab
    , -- The Wages of Sin
      SomeEnemyCard heretic_A
    , SomeEnemyCard heretic_C
    , SomeEnemyCard heretic_E
    , SomeEnemyCard heretic_G
    , SomeEnemyCard heretic_I
    , SomeEnemyCard heretic_K
    , SomeEnemyCard vengefulWitch
    , SomeEnemyCard malevolentSpirit
    , SomeEnemyCard reanimatedDead
    , -- For the Greater Good
      SomeEnemyCard nathanWickMasterOfInitiation
    , SomeEnemyCard nathanWickMasterOfIndoctrination
    , SomeEnemyCard lodgeJailor
    , SomeEnemyCard cellKeeper
    , SomeEnemyCard summonedBeast
    , SomeEnemyCard knightOfTheInnerCircle
    , SomeEnemyCard knightOfTheOuterVoid
    , -- Union and Disillusion
      SomeEnemyCard gavriellaMizrah
    , SomeEnemyCard jeromeDavids
    , SomeEnemyCard pennyWhite
    , SomeEnemyCard valentinoRivas
    , SomeEnemyCard whippoorwillUnionAndDisillusion
    , SomeEnemyCard spectralRaven
    , -- In the Clutches of Chaos
      --- Music of the Damned
      SomeEnemyCard anetteMasonReincarnatedEvil
    , SomeEnemyCard witnessOfChaos
    , --- Secrets of the Universe
      SomeEnemyCard carlSanfordDeathlessFanatic
    , SomeEnemyCard lodgeEnforcer
    , -- Before the Black Throne
      SomeEnemyCard mindlessDancer
    , SomeEnemyCard azathoth
    , -- The Dream-Eaters
      -- signature
      SomeEnemyCard tonysQuarry
    , SomeEnemyCard watcherFromAnotherDimension
    , -- rogue
      SomeEnemyCard guardianOfTheCrystallizer
    , -- basic weakness
      SomeEnemyCard yourWorstNightmare
    , --- Where the Gods Dwell
      -- mystic
      SomeEnemyCard unboundBeast
    , --- Beyond the Gates of Sleep
      SomeEnemyCard kamanThah
    , SomeEnemyCard nasht
    , SomeEnemyCard laboringGug
    , SomeEnemyCard ancientZoog
    , --- Waking Nightmare
      SomeEnemyCard suspiciousOrderly
    , SomeEnemyCard corruptedOrderly
    , --- Agents of Atlach-Nacha
      SomeEnemyCard greyWeaver
    , --- Agents of Nyarlathotep
      SomeEnemyCard theCrawlingMist
    , --- Creatures of the Underworld
      SomeEnemyCard huntingGhast
    , SomeEnemyCard lumberingGug
    , --- Spiders
      SomeEnemyCard spiderOfLeng
    , SomeEnemyCard swarmOfSpiders
    , --- Corsairs
      SomeEnemyCard corsairOfLeng
    , --- Zoogs
      SomeEnemyCard furtiveZoog
    , SomeEnemyCard stealthyZoog
    , SomeEnemyCard inconspicuousZoog
    , -- The Search For Kadath
      SomeEnemyCard catsOfUlthar
    , SomeEnemyCard stalkingManticore
    , SomeEnemyCard hordeOfNight
    , SomeEnemyCard beingsOfIb
    , SomeEnemyCard priestOfAThousandMasks
    , SomeEnemyCard tenebrousNightgaunt
    , SomeEnemyCard packOfVooniths
    , SomeEnemyCard nightriders
    , -- A Thousand Shapes of Horror
      SomeEnemyCard theUnnamable
    , -- Dark Side of the Moon
      SomeEnemyCard moonLizard
    , SomeEnemyCard moonboundByakhee
    , SomeEnemyCard catsFromSaturn
    , SomeEnemyCard moonBeast
    , -- Point of No Return
      SomeEnemyCard gugSentinel
    , --- Terror of the Vale
      SomeEnemyCard slitheringDhole
    , --- Descent into the Pitch
      SomeEnemyCard pitchSpider
    , -- Where the Gods Dwell
      SomeEnemyCard nyarlathotepTheCrawlingChaos
    , SomeEnemyCard nyarlathotepTheFacelessWhisperer
    , SomeEnemyCard nyarlathotepMessengerOfTheOuterGods
    , SomeEnemyCard nyarlathotepGodOfAThousandForms
    , SomeEnemyCard nyarlathotepStalkerAmongTheStars
    , SomeEnemyCard nyarlathotepTrueShape
    , SomeEnemyCard highPriestNotToBeDescribed
    , SomeEnemyCard dholeOfTheWastes
    , SomeEnemyCard liarWithNoFace
    , -- Weaver of the Cosmos
      SomeEnemyCard atlachNacha
    , SomeEnemyCard legsOfAtlachNacha_347
    , SomeEnemyCard legsOfAtlachNacha_348
    , SomeEnemyCard legsOfAtlachNacha_349
    , SomeEnemyCard legsOfAtlachNacha_350
    , SomeEnemyCard webSpinner
    , -- The Innsmouth Conspiracy
      --- signature [tic]
      SomeEnemyCard shadowAgents
    , --- basic weakness [tic]
      SomeEnemyCard accursedFollower
    , --- The Pit of Despair [tic]
      SomeEnemyCard theAmalgam
    , --- The Vanishing of Elina Harper [tic]
      SomeEnemyCard angryMob
    , SomeEnemyCard robertFriendlyDisgruntledDockworker
    , SomeEnemyCard zadokAllenDrunkAndDisorderly
    , SomeEnemyCard brianBurnhamWantsOut
    , SomeEnemyCard barnabasMarshTheChangeIsUponHim
    , SomeEnemyCard joyceLittleBookshopOwner
    , SomeEnemyCard otheraGilmanProprietessOfTheHotel
    , --- Agents of Dagon [tic]
      SomeEnemyCard priestOfDagon
    , SomeEnemyCard initiateOfDagon
    , --- Agents of Hydra [tic]
      SomeEnemyCard lloigor
    , --- Creatures of the Deep [tic]
      SomeEnemyCard deepOneBull
    , SomeEnemyCard lurkingDeepOne
    , --- Fog Over Innsmouth [tic]
      SomeEnemyCard wingedOneFogOverInnsmouth
    , --- The Locals [tic]
      SomeEnemyCard innsmouthTroublemaker
    , --- In Too Deep [itd]
      SomeEnemyCard innsmouthShoggoth
    , SomeEnemyCard ravagerFromTheDeep
    , SomeEnemyCard emergingDeepOne
    , --- Devil Reef [def]
      SomeEnemyCard theTerrorOfDevilReef_164
    , SomeEnemyCard theTerrorOfDevilReef_165
    , SomeEnemyCard deepOnePredator
    , SomeEnemyCard huntingDeepOne
    , --- Horror in High Gear [hhg]
      SomeEnemyCard theTerrorOfDevilReefRelentlessMonstrosity
    , SomeEnemyCard pursuingMotorcar
    , SomeEnemyCard hitVan
    , SomeEnemyCard hybridAssassin
    , --- A Light in the Fog [lif]
      SomeEnemyCard oceirosMarsh
    , SomeEnemyCard deepOneNursemaid
    , SomeEnemyCard deepOneHatchling
    , --- The Lair of Dagon [lod]
      SomeEnemyCard dagonDeepInSlumber
    , SomeEnemyCard dagonAwakenedAndEnraged
    , SomeEnemyCard apostleOfDagon
    , SomeEnemyCard cerenerianDeepOne
    , --- Into the Maelstrom [itm]
      SomeEnemyCard dagonDeepInSlumberIntoTheMaelstrom
    , SomeEnemyCard dagonAwakenedAndEnragedIntoTheMaelstrom
    , SomeEnemyCard hydraDeepInSlumber
    , SomeEnemyCard hydraAwakenedAndEnraged
    , SomeEnemyCard aquaticAbomination
    , SomeEnemyCard dagonsBrood
    , SomeEnemyCard hydrasBrood
    , -- Edge of the Earth
      -- signature
      SomeEnemyCard mobGoons
    , -- Ice and Death [eote]
      SomeEnemyCard skitteringNonsense
    , -- The Crash [eote]
      SomeEnemyCard terrorOfTheStarsBringerOfIceAndDeath
    , -- Lost in the Night [eote]
      SomeEnemyCard professorWilliamDyerProfessorOfGeology
    , SomeEnemyCard danforthBrilliantStudent
    , SomeEnemyCard eliyahAshevakDogHandler
    , SomeEnemyCard drMalaSinhaDaringPhysician
    , SomeEnemyCard averyClaypoolAntarcticGuide
    , SomeEnemyCard jamesCookieFredericksDubiousChoice
    , SomeEnemyCard drAmyKenslerProfessorOfBiology
    , SomeEnemyCard roaldEllsworthIntrepidExplorer
    , SomeEnemyCard takadaHirokoAeroplaneMechanic
    , -- Seeping Nightmares [eote]
      SomeEnemyCard seepingNightmare
    , -- Fatal Mirage [eote]
      SomeEnemyCard memoryOfAHuntGoneAwry
    , SomeEnemyCard memoryOfALostPatient
    , SomeEnemyCard memoryOfAMissingFather
    , SomeEnemyCard memoryOfARavagedCountry
    , SomeEnemyCard memoryOfARegretfulVoyage
    , SomeEnemyCard memoryOfAnUnspeakableEvil
    , SomeEnemyCard memoryOfATerribleDiscovery
    , SomeEnemyCard memoryOfAnAlienTranslation
    , SomeEnemyCard memoryOfAnUnrequitedLove
    , SomeEnemyCard horrifyingShade
    , -- To the Forbidden Peaks [eote]
      SomeEnemyCard terrorOfTheStarsGuardianOfForbiddenPeaks
    , SomeEnemyCard constrictingElderThing
    , -- City of the Elder Things [eote]
      SomeEnemyCard terrorOfTheStarsBaneOfTheElderThings
    , SomeEnemyCard benignElderThing
    , SomeEnemyCard reawakenedElderThing
    , -- The Great Seal [eote]
      SomeEnemyCard protoplasmicMass
    , -- Stirring in the Deep [eote]
      SomeEnemyCard theNamelessMadness
    , SomeEnemyCard unsealedPhantasm
    , -- Agents of the Unknown [eote]
      SomeEnemyCard primordialEvil
    , -- Creatures in the Ice [eote]
      SomeEnemyCard manifestationOfMadness
    , SomeEnemyCard glacialPhantasm
    , -- Elder Things [eote]
      SomeEnemyCard elderThingScavenger
    , SomeEnemyCard guardianElderThing
    , -- Left Behind [eote]
      SomeEnemyCard lostResearcher
    , SomeEnemyCard frenziedExplorer
    , -- Penguins [eote]
      SomeEnemyCard giantAlbinoPenguin
    , -- Shoggoths [eote]
      SomeEnemyCard forgottenShoggoth
    , SomeEnemyCard rampagingShoggoth
    , -- The Scarlet Keys
      -- signature
      SomeEnemyCard agentFletcher
    , -- basic weakness
      SomeEnemyCard lurkerInTheDark
    , SomeEnemyCard ectoplasmicHorror
    , -- The Feast of Hemloch Vale
      -- signature [fhv]
      SomeEnemyCard zamacona
    , SomeEnemyCard weepingYurei
    , -- rogue (bonded) [fhv]
      SomeEnemyCard biancaDieKatz
    , -- Return to Night of the Zealot
      -- Return to the Gathering
      SomeEnemyCard corpseHungryGhoul
    , SomeEnemyCard ghoulFromTheDepths
    , -- Return to the Midnight Masks
      SomeEnemyCard narogath
    , -- Ghouls of Umordhoth
      SomeEnemyCard graveEater
    , SomeEnemyCard acolyteOfUmordhoth
    , -- The Devourer's Cult
      SomeEnemyCard discipleOfTheDevourer
    , SomeEnemyCard corpseTaker
    , -- Return to Cult of Umordhoth
      SomeEnemyCard jeremiahPierce
    , SomeEnemyCard billyCooper
    , SomeEnemyCard almaHill
    , -- Return to the Dunwich Legacy
      -- Return to Extracurricular Activities
      SomeEnemyCard enthralledSecurityGuard
    , -- Return to Extracurricular Activities
      SomeEnemyCard theConductorBeastFromBeyondTheGate
    , -- Return to Blood on the Altar
      SomeEnemyCard hiredGun
    , -- Return to Undimensioned and Unseen
      SomeEnemyCard broodOfYogSothothChargingBeast
    , SomeEnemyCard broodOfYogSothothThrashingSpawn
    , SomeEnemyCard broodOfYogSothothSwellingDevourer
    , SomeEnemyCard broodOfYogSothothAmorphousTerror
    , -- Return to Lost in Time and Space
      SomeEnemyCard sethBishopThrallOfYogSothoth
    , -- Yog Sothoth's Emissaries
      SomeEnemyCard vassalOfTheLurker
    , -- Return to the Path to Carcosa
      --- Return to Curtain Call [rtptc]
      SomeEnemyCard laComtesseSubverterOfPlans
    , --- Return to the Last King [rtptc]
      SomeEnemyCard dianneDevineKnowsWhatYoureUpTo
    , SomeEnemyCard crazedGuest
    , --- Return to Echoes of the Past [rtptc]
      SomeEnemyCard keeperOfTheOath
    , --- Return to The Unspeakable Oath [rtptc]
      SomeEnemyCard hostOfInsanity
    , --- Return to The Pallid Mask [rtptc]
      SomeEnemyCard malformedSkeleton
    , --- Return to Dim Carcosa [rtptc]
      SomeEnemyCard highPriestOfHastur
    , --- Decaying Reality [rtptc]
      SomeEnemyCard maggotSwarm
    , --- Hastur's Envoys [rtptc]
      SomeEnemyCard preyingByakhee
    , -- Return to The Forgotten Age
      --- Return to The Doom of Eztli [rttfa]
      SomeEnemyCard harbingerOfValusiaTheSleeperReturns
    , --- Return to Pillars of Judgement [rttfa]
      SomeEnemyCard theWingedSerpentTheFuryOfYig
    , SomeEnemyCard featheredSerpent
    , --- Cult of Pnakotus [rttfa]
      SomeEnemyCard brotherhoodAcolyte
    , SomeEnemyCard stolenMind
    , --- Temporal Hunters [rttfa]
      SomeEnemyCard tindalosAlpha
    , --- Venomous Hate [rttfa]
      SomeEnemyCard vengefulSerpent
    , SomeEnemyCard serpentGuardian
    , -- Nathanial Cho
      SomeEnemyCard tommyMalloy
    , -- Curse of the Rougarou
      SomeEnemyCard bogGator
    , SomeEnemyCard swampLeech
    , SomeEnemyCard theRougarou
    , SomeEnemyCard slimeCoveredDhole
    , SomeEnemyCard marshGug
    , SomeEnemyCard darkYoungHost
    , -- Carnevale of Horrors
      SomeEnemyCard balefulReveler
    , SomeEnemyCard donLagorio
    , SomeEnemyCard elisabettaMagro
    , SomeEnemyCard salvatoreNeri
    , SomeEnemyCard savioCorvi
    , SomeEnemyCard cnidathqua
    , SomeEnemyCard poleman
    , SomeEnemyCard carnevaleSentinel
    , SomeEnemyCard writhingAppendage
    , -- Murder at the Excelsior Hotel
      SomeEnemyCard arkhamOfficer
    , SomeEnemyCard mrTrombly
    , SomeEnemyCard conspicuousStaff
    , SomeEnemyCard hotelGuest
    , --- Alien Interference
      SomeEnemyCard otherworldlyMeddler
    , --- Excelsior Management
      SomeEnemyCard hotelManager
    , SomeEnemyCard hotelSecurity
    , --- Dark Rituals
      SomeEnemyCard dimensionalShambler
    , SomeEnemyCard cultistOfTheEnclave
    , --- Sins of the Past
      SomeEnemyCard vengefulSpecter
    , -- Parallel
      --- Laid to Rest
      SomeEnemyCard vengefulShade
    , -- Promo
      --- Hour of the Huntress
      SomeEnemyCard sacrificialBeast
    , --- Ire of the Void
      SomeEnemyCard vengefulHound
    , --- Aura of Faith
      SomeEnemyCard serpentsOfYigAdvanced
    , -- The Midwinter Gala
      SomeEnemyCard abhorrentMoonBeast
    , SomeEnemyCard declanPearce
    , SomeEnemyCard enragedGug
    , SomeEnemyCard lanternClubMember
    , SomeEnemyCard valeriyaAntonovaDontMessWithHer
    , SomeEnemyCard rookieCop
    , SomeEnemyCard caldwellPhilipsCompelledByDreams
    , SomeEnemyCard johnnyValoneHereToCollect
    , SomeEnemyCard carlSanfordIntimidatingPresence
    , SomeEnemyCard williamBainDefiantToTheLast
    , SomeEnemyCard savageShantak
    , SomeEnemyCard theBloodlessMan
    , SomeEnemyCard theBloodlessManUnleashed
    ]
