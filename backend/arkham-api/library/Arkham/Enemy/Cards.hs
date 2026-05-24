module Arkham.Enemy.Cards (module Arkham.Enemy.Cards, module X) where

import Arkham.Enemy.CardDefs.Base as X
import Arkham.Enemy.CardDefs.Core2026 as X
import Arkham.Enemy.CardDefs.EdgeOfTheEarth as X
import Arkham.Enemy.CardDefs.NightOfTheZealot as X
import Arkham.Enemy.CardDefs.Promo as X
import Arkham.Enemy.CardDefs.ReturnTo as X
import Arkham.Enemy.CardDefs.Standalone as X
import Arkham.Enemy.CardDefs.TheCircleUndone as X
import Arkham.Enemy.CardDefs.TheDreamEaters as X
import Arkham.Enemy.CardDefs.TheDunwichLegacy as X
import Arkham.Enemy.CardDefs.TheFeastOfHemlockVale as X
import Arkham.Enemy.CardDefs.TheForgottenAge as X
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy as X
import Arkham.Enemy.CardDefs.ThePathToCarcosa as X
import Arkham.Enemy.CardDefs.TheScarletKeys as X

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.EncounterSet hiding (Blight, Byakhee, Dreamlands)
import Arkham.Keyword qualified as Keyword
import Arkham.Name
import Arkham.Prelude
import Arkham.Trait

{- HLint ignore "Use camelCase" -}

allPlayerEnemyCards :: Map CardCode CardDef
allPlayerEnemyCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [ mobEnforcer
      , silverTwilightAcolyte
      , stubbornDetective
      , graveyardGhouls
      , theThingThatFollows
      , theManInThePallidMask
      , serpentsOfYig
      , hoods
      , tonysQuarry
      , watcherFromAnotherDimension
      , guardianOfTheCrystallizer
      , yourWorstNightmare
      , unboundBeast
      , shadowAgents
      , accursedFollower
      , mobGoons
      , agentFletcher
      , lurkerInTheDark
      , ectoplasmicHorror
      , zamacona
      , weepingYurei
      , biancaDieKatz
      , blackChamberOperative
      , theNamelessLurker
      , tommyMalloy
      , vengefulShade
      , sacrificialBeast
      , vengefulHound
      , serpentsOfYigAdvanced
      , felineHybrid
      , bloodDrinker
      ]

allEncounterEnemyCards :: Map CardCode CardDef
allEncounterEnemyCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [ abarranArrigorriagakoaAbarranUnleashed
      , abarranArrigorriagakoaTheManWithTheRubyRing
      , abhorrentMoonBeast
      , acolyte
      , acolyteOfUmordhoth
      , agentOfTheKing
      , alejandroVela
      , alikiZoniUperetriaSpeaksInDeath
      , allosaurusIndomitablePredator
      , allosaurusRampagingPredator
      , almaHill
      , amaranthCorruptionRevealed
      , amaranthLurkingCorruption
      , amaranthScarletScorn
      , ancientRaider
      , ancientZoog
      , anetteMason
      , anetteMasonReincarnatedEvil
      , angryMob
      , apexStrangleweed
      , apocalypticPresage
      , apostleOfDagon
      , apportionedKa
      , aquaticAbomination
      , arkhamOfficer
      , ashleighClarke
      , asylumGorger
      , atlachNacha
      , averyClaypoolAntarcticGuide
      , avianThrall
      , azathoth
      , balefulReveler
      , barnabasMarshTheChangeIsUponHim
      , basilisk
      , beastOfAldebaran
      , beingsOfIb
      , benignElderThing
      , billyCooper
      , blackAmanita
      , boaConstrictor
      , bogGator
      , boundNightgaunt
      , brianBurnhamWantsOut
      , broodOfYig
      , broodOfYogSothoth
      , broodOfYogSothothAmorphousTerror
      , broodOfYogSothothChargingBeast
      , broodOfYogSothothSwellingDevourer
      , broodOfYogSothothThrashingSpawn
      , broodQueenDyingMother
      , broodSoldier
      , brotherhoodAcolyte
      , brotherhoodCultist
      , brownJenkin
      , buriedMinerALostMemento
      , buriedMinerExhumeTheBones
      , burrowingHybrid
      , captiveSubjects
      , carlSanfordDeathlessFanatic
      , carnevaleSentinel
      , casinoGuardA
      , casinoGuardB
      , casinoGuardC
      , catacombsDocent
      , catsFromSaturn
      , catsOfUlthar
      , cellKeeper
      , cerenerianDeepOne
      , cloverClubPitBoss
      , cnidathqua
      , cochlealStag
      , colorlessLarva
      , conglomerationOfSpheres
      , conspicuousStaff
      , constanceDumaine
      , constrictingElderThing
      , corpseDweller
      , corpseHungryGhoul
      , corpseLichen
      , corpseTaker
      , corruptedOrderly
      , corsairOfLeng
      , coterieAgentA
      , coterieAgentB
      , coterieAgentC
      , coterieAssassinA
      , coterieAssassinB
      , coterieEnforcerA
      , coterieEnforcerB
      , coterieEnvoy
      , coterieProvocateur
      , covenInitiate
      , crazedGuest
      , crazedShoggoth
      , creatureOutOfDemhe
      , crystalParasite
      , cultistOfTheEnclave
      , curiousMoonNosyNuisance
      , dagonAwakenedAndEnraged
      , dagonAwakenedAndEnragedIntoTheMaelstrom
      , dagonDeepInSlumber
      , dagonDeepInSlumberIntoTheMaelstrom
      , dagonsBrood
      , danforthBrilliantStudent
      , danielChesterfield
      , darkYoungHost
      , declanPearce
      , desiderioDelgadoAlvarez106
      , desiderioDelgadoAlvarez107
      , desiderioDelgadoAlvarezRedInHisLedger
      , dmitriKonstantinovTakingTheLongView
      , caldwellPhilipsCompelledByDreams
      , carlSanfordIntimidatingPresence
      , valeriyaAntonovaDontMessWithHer
      , deepOneBull
      , deepOneHatchling
      , deepOneNursemaid
      , deepOnePredator
      , devoteeOfTheKey
      , dholeOfTheWastes
      , dianneDevine
      , dianneDevineKnowsWhatYoureUpTo
      , dimensionalDisplacerA
      , dimensionalDisplacerB
      , dimensionalDuplicatorA
      , dimensionalDuplicatorB
      , dimensionalShambler
      , dimensionalShamblerHunterFromBeyond
      , discipleOfTheDevourer
      , donLagorio
      , drAmyKenslerProfessorOfBiology
      , drMalaSinhaDaringPhysician
      , dromaeosaurus
      , eaterOfTheDepths
      , elderThingScavenger
      , elisabettaMagro
      , eliyahAshevakDogHandler
      , emergentMonstrosity
      , emergingDeepOne
      , emissaryFromYuggoth
      , enragedGug
      , enthralledSecurityGuard
      , erikaStrandPossessedProducer
      , eztliGuardian
      , fanatic
      , fangOfYig
      , featheredSerpent
      , fleshEater
      , forestWatcher
      , forgottenShoggoth
      , formlessSpawn
      , fortunesDaggerA
      , fortunesDaggerB
      , fortunesShieldA
      , fortunesShieldB
      , frenziedExplorer
      , frenziedMiner
      , furtiveZoog
      , gavriellaMizrah
      , ghostLight
      , ghoulFromTheDepths
      , ghoulMinion
      , ghoulPriest
      , giantAlbinoPenguin
      , glacialPhantasm
      , goatSpawn
      , grapplingHorror
      , grapplingSpawn
      , graveEater
      , greyWeaver
      , guardianElderThing
      , gugSentinel
      , handOfTheBrotherhood
      , harbingerOfValusia
      , harbingerOfValusiaTheSleeperReturns
      , harlanEarnstoneCrazedByTheCurse
      , hasturLordOfCarcosa
      , hasturTheKingInYellow
      , hasturTheTatteredKing
      , henryDeveauAlejandrosKidnapper
      , heretic_A
      , heretic_C
      , heretic_E
      , heretic_G
      , heretic_I
      , heretic_K
      , hermanCollins
      , highPriestNotToBeDescribed
      , highPriestOfHastur
      , hiredGun
      , hitVan
      , hordeOfNight
      , horrifyingShade
      , hostOfInsanity
      , hotelGuest
      , hotelManager
      , hotelSecurity
      , houseDealerA
      , houseDealerB
      , huntingDeepOne
      , huntingGhast
      , huntingHorror
      , huntingNightgaunt
      , hybridAssassin
      , hydraAwakenedAndEnraged
      , hydraDeepInSlumber
      , hydrasBrood
      , ichtaca
      , ichtacaScionOfYig
      , icyGhoul
      , inconspicuousZoog
      , initiateOfDagon
      , innsmouthShoggoth
      , innsmouthTroublemaker
      , interstellarTraveler
      , ishimaruHaruko
      , jamesCookieFredericksDubiousChoice
      , jeremiahPierce
      , jeromeDavids
      , jordanPerry
      , josefMeiger
      , johnnyValoneHereToCollect
      , joyceLittleBookshopOwner
      , kamanThah
      , keeperOfSecrets
      , keeperOfTheGreatLibrary
      , keeperOfTheOath
      , khalidBelovedCompanion
      , knightOfTheInnerCircle
      , knightOfTheOuterVoid
      , laChicaRojaHotOnYourTrail
      , laChicaRojaTheGirlInTheCarmineCoat
      , laComtesseSubverterOfPlans
      , laboringGug
      , legsOfAtlachNacha_347
      , legsOfAtlachNacha_348
      , legsOfAtlachNacha_349
      , legsOfAtlachNacha_350
      , liarWithNoFace
      , lloigor
      , lodgeEnforcer
      , lodgeJailor
      , lodgeNeophyte
      , lanternClubMember
      , lostResearcher
      , lumberingGug
      , lupineThrall
      , lurkingDeepOne
      , madPatient
      , maggotSwarm
      , malevolentSpirit
      , malformedSkeleton
      , maniac
      , manifestationOfMadness
      , mariaDeSilvaKnowsMoreThanSheLetsOn
      , marshGug
      , memoryOfAHuntGoneAwry
      , memoryOfALostPatient
      , memoryOfAMissingFather
      , memoryOfARavagedCountry
      , memoryOfARegretfulVoyage
      , memoryOfATerribleDiscovery
      , memoryOfAnAlienTranslation
      , memoryOfAnUnrequitedLove
      , memoryOfAnUnspeakableEvil
      , miasmaticShadow
      , mimeticNemesisInfiltratorOfRealities
      , mimeticNemesisOtherworldlySubjugator
      , mindlessDancer
      , mrTrombly
      , mobster
      , moonBeast
      , moonLizard
      , moonboundByakhee
      , nahab
      , narogath
      , nasht
      , nathanWickMasterOfIndoctrination
      , nathanWickMasterOfInitiation
      , netherMist
      , nightriders
      , nyarlathotepGodOfAThousandForms
      , nyarlathotepMessengerOfTheOuterGods
      , nyarlathotepStalkerAmongTheStars
      , nyarlathotepTheCrawlingChaos
      , nyarlathotepTheFacelessWhisperer
      , oBannionsThug
      , oceirosMarsh
      , otheraGilmanProprietessOfTheHotel
      , otherworldlyMeddler
      , otherworldlyMimic
      , packOfVooniths
      , padmaAmrita
      , paracausalEntity
      , paradigmEfficer
      , pennyWhite
      , peterWarren
      , piperOfAzathoth
      , pitViper
      , pitWarden
      , pitchSpider
      , poisonblossom
      , poleman
      , poltergeist
      , possessedExtra_19
      , possessedExtra_20
      , possessedExtra_21
      , possessedOathspeaker
      , preyingByakhee
      , priestOfAThousandMasks
      , priestOfDagon
      , priestessOfTheCoven
      , primordialEvil
      , professorWilliamDyerProfessorOfGeology
      , protoplasmicMass
      , protoplasmicReassembler
      , pursuingMotorcar
      , rampagingShoggoth
      , ravagerFromTheDeep
      , ravenousGhoul
      , ravenousGrizzly
      , razinFarhiReanimatedArtificer
      , reawakenedElderThing
      , relentlessDarkYoung
      , returnToHeretic_38
      , returnToHeretic_39
      , riftSeeker
      , roachSwarm
      , roaldEllsworthIntrepidExplorer
      , robertFriendlyDisgruntledDockworker
      , royalEmissary
      , rookieCop
      , ruthTurner
      , salvatoreNeri
      , saturniteDrudgeMilitia
      , saturniteMonarchGraciousHost
      , saturniteMonarchInAnAlienLand
      , savageShantak
      , savioCorvi
      , scarletBeast
      , scholarFromYith
      , scientistOfYith
      , screechingBanshee
      , screechingByakhee
      , sebastienMoreau
      , securityPatrolA
      , securityPatrolB
      , securityPatrolC
      , seekerOfCarcosa
      , seepingNightmare
      , senatorNathanielRhodesAdeptPolitician
      , serpentFromYoth
      , serpentGuardian
      , serpentOfTenochtitlan
      , servantOfManyMouths
      , servantOfTheLurker
      , sethBishop
      , sethBishopThrallOfYogSothoth
      , shadowHound
      , silasBishop
      , sinisterAspirantA
      , sinisterAspirantB
      , sinisterAspirantC
      , skitteringNonsense
      , slainForemanFamilialPain
      , slainForemanSympathyPain
      , slimeCoveredDhole
      , slitheringDhole
      , spawnOfHali
      , specterOfDeath
      , spectralRaven
      , spiderOfLeng
      , stalkingHybrid
      , stalkingManticore
      , stealthyByakhee
      , stealthyZoog
      , stolenMind
      , subterraneanBeast
      , summonedBeast
      , suspiciousOrderly
      , swampLeech
      , swarmOfRats
      , swarmOfSpiders
      , swiftByakhee
      , takadaHirokoAeroplaneMechanic
      , temporalDevourer
      , tenebrousNightgaunt
      , terrorOfTheStarsBaneOfTheElderThings
      , terrorOfTheStarsBringerOfIceAndDeath
      , terrorOfTheStarsGuardianOfForbiddenPeaks
      , theAmalgam
      , theBeastInACowlOfCrimsonLeavingATrailOfDestruction
      , theBeastInACowlOfCrimsonRavagerInRed
      , theBeastInACowlOfCrimsonWolfInSheepsClothing
      , theBloodlessMan
      , theBloodlessManUnleashed
      , theClaretKnightCoterieKingpin
      , theClaretKnightHoldsYouInContempt
      , theContessaEnraged
      , theContessaNeedlesslySmug
      , theCrawlingMist
      , theConductorBeastFromBeyondTheGate
      , theExperiment
      , theMaskedHunter
      , theNamelessMadness
      , theOrganistDrapedInMystery
      , theOrganistHopelessIDefiedHim
      , theRedGlovedManPurposeUnknown
      , theRedGlovedManShroudedInMystery
      , theSanguineWatcherWithTheRubySpectacles
      , theSanguineWatcherHeSeesWhatIsNotThere
      , theSpectralWatcher
      , theTerrorOfDevilReef_164
      , theTerrorOfDevilReef_165
      , theTerrorOfDevilReefRelentlessMonstrosity
      , theRougarou
      , theUnnamable
      , theWingedSerpent
      , theWingedSerpentTheFuryOfYig
      , thorneOpenToNegotiation
      , thorneTheOneWithTheRedCravat
      , thrall
      , thrallDeadHeat
      , tidalTerror
      , tindalosAlpha
      , tzuSanNiangAWhisperInYourEar
      , tzuSanNiangOutForBlood
      , tzuSanNiangTheLadyWithTheRedParasol
      , umbralHarbinger
      , umordhoth
      , uncannyShadowPlayfulShadows
      , uncannyShadowTimorousShadows
      , unsealedPhantasm
      , ursineHybridGlowingAbomination
      , valentinoRivas
      , vampireThrall
      , vassalOfTheLurker
      , vengefulSerpent
      , vengefulSpecter
      , vengefulWitch
      , victoriaDevereux
      , voidChimeraEarsplitter
      , voidChimeraFellbeak
      , voidChimeraFellhound
      , voidChimeraGorefeaster
      , voidChimeraTrueForm
      , webSpinner
      , werewolf
      , whippoorwill
      , whippoorwill2
      , whippoorwillUnionAndDisillusion
      , wingedOne
      , wingedOneFogOverInnsmouth
      , witnessOfChaos
      , wizardOfTheOrder
      , wizardOfYogSothoth
      , williamBainDefiantToTheLast
      , wolfManDrew
      , wraith
      , writhingAppendage
      , yig
      , yithianObserver
      , yithianStarseeker
      , yogSothoth
      , youngDeepOne
      , youngPsychopath
      , zadokAllenDrunkAndDisorderly
      , servantOfFlameRagingFury
      , servantOfFlameOnTheRun
      , servantOfFlameAWillingSacrifice
      , cantorOfFlame
      , hellhound
      , bystander
      , mutatedExperiment
      , batHorror
      , rogueGangster
      , zealot
      , darkMagician
      , queensKnight
      , heraldOfFlame
      , elokossFaintEmbers
      , elokossMotherOfFlame
      , davidRenfieldDisillusionedEschatologist
      , corneliaAkelyExhaustedSupervisor
      , naomiOBannionRunsThisTown
      , sgtEarlMonroeDirtyCop
      , abigailForemanWaryLibrarian
      , margaretLiuBeguilingLoungeSinger
      ]

allSpecialEnemyCards :: Map CardCode CardDef
allSpecialEnemyCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [flyingPolyp, reanimatedDead, nyarlathotepTrueShape, golem, extradimensionalEnemy]

flyingPolyp :: CardDef
flyingPolyp =
  (enemy "xpolyp" "Flying Polyp" ShatteredAeons 0)
    { cdCardTraits = singleton Monster
    }

reanimatedDead :: CardDef
reanimatedDead =
  (enemy "xreanimated" "Reanimated Dead" TheWagesOfSin 0)
    { cdCardTraits = singleton Monster
    }

nyarlathotepTrueShape :: CardDef
nyarlathotepTrueShape =
  unique
    $ (enemy "xnyarlathotep" ("Nyarlathotep" <:> "True Shape") WhereTheGodsDwell 0)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdVictoryPoints = Just 0
      }

golem :: CardDef
golem =
  (enemy "xgolem" "Golem" WithoutATrace 0)
    { cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = singleton Keyword.Hunter
    }

extradimensionalEnemy :: CardDef
extradimensionalEnemy =
  (enemy "xextra" "Extradimensional Enemy" FortuneAndFolly 0)
    { cdCardTraits = singleton Extradimensional
    }
