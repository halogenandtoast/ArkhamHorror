module Arkham.Enemy.Cards where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding (Byakhee, Dreamlands)
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Name
import Arkham.Trait

{- HLint ignore "Use camelCase" -}

baseEnemy
  :: CardCode
  -> Name
  -> Maybe (EncounterSet, Int)
  -> Maybe CardSubType
  -> CardDef
baseEnemy cardCode name mEncounterSet isWeakness =
  (emptyCardDef cardCode name $ if isJust isWeakness then PlayerEnemyType else EnemyType)
    { cdCardSubType = isWeakness
    , cdClassSymbols = if isJust isWeakness then singleton Neutral else mempty
    , cdEncounterSet = fst <$> mEncounterSet
    , cdEncounterSetQuantity = snd <$> mEncounterSet
    , cdLevel = Nothing
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

doubleSided :: CardCode -> CardDef -> CardDef
doubleSided cCode def =
  def
    { cdDoubleSided = True
    , cdOtherSide = Just cCode
    }

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseEnemy cardCode name Nothing (Just Weakness)

basicWeakness :: CardCode -> Name -> CardDef
basicWeakness cardCode name =
  baseEnemy cardCode name Nothing (Just BasicWeakness)

enemy :: CardCode -> Name -> EncounterSet -> Int -> CardDef
enemy cardCode name encounterSet encounterSetQuantity =
  baseEnemy cardCode name (Just (encounterSet, encounterSetQuantity)) Nothing

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
      , tommyMalloy
      , vengefulShade
      , sacrificialBeast
      , vengefulHound
      , serpentsOfYigAdvanced
      ]

allEncounterEnemyCards :: Map CardCode CardDef
allEncounterEnemyCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [ abhorrentMoonBeast
      , acolyte
      , acolyteOfUmordhoth
      , agentOfTheKing
      , alejandroVela
      , almaHill
      , ancientZoog
      , anetteMason
      , anetteMasonReincarnatedEvil
      , angryMob
      , apexStrangleweed
      , apostleOfDagon
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
      , boaConstrictor
      , bogGator
      , brianBurnhamWantsOut
      , broodOfYig
      , broodOfYogSothoth
      , broodOfYogSothothAmorphousTerror
      , broodOfYogSothothChargingBeast
      , broodOfYogSothothSwellingDevourer
      , broodOfYogSothothThrashingSpawn
      , brotherhoodAcolyte
      , brotherhoodCultist
      , brownJenkin
      , carlSanfordDeathlessFanatic
      , carnevaleSentinel
      , catacombsDocent
      , catsFromSaturn
      , catsOfUlthar
      , cellKeeper
      , cerenerianDeepOne
      , cloverClubPitBoss
      , cnidathqua
      , conglomerationOfSpheres
      , conspicuousStaff
      , constanceDumaine
      , constrictingElderThing
      , corpseDweller
      , corpseHungryGhoul
      , corpseTaker
      , corruptedOrderly
      , corsairOfLeng
      , covenInitiate
      , crazedGuest
      , crazedShoggoth
      , creatureOutOfDemhe
      , cultistOfTheEnclave
      , dagonAwakenedAndEnraged
      , dagonAwakenedAndEnragedIntoTheMaelstrom
      , dagonDeepInSlumber
      , dagonDeepInSlumberIntoTheMaelstrom
      , dagonsBrood
      , danforthBrilliantStudent
      , danielChesterfield
      , darkYoungHost
      , declanPearce
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
      , dimensionalShambler
      , discipleOfTheDevourer
      , donLagorio
      , drAmyKenslerProfessorOfBiology
      , drMalaSinhaDaringPhysician
      , eaterOfTheDepths
      , elderThingScavenger
      , elisabettaMagro
      , eliyahAshevakDogHandler
      , emergentMonstrosity
      , emergingDeepOne
      , enragedGug
      , enthralledSecurityGuard
      , eztliGuardian
      , fanatic
      , fangOfYig
      , featheredSerpent
      , fleshEater
      , forgottenShoggoth
      , formlessSpawn
      , frenziedExplorer
      , furtiveZoog
      , gavriellaMizrah
      , ghoulFromTheDepths
      , ghoulMinion
      , ghoulPriest
      , giantAlbinoPenguin
      , glacialPhantasm
      , goatSpawn
      , grapplingHorror
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
      , knightOfTheInnerCircle
      , knightOfTheOuterVoid
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
      , packOfVooniths
      , padmaAmrita
      , pennyWhite
      , peterWarren
      , piperOfAzathoth
      , pitViper
      , pitWarden
      , pitchSpider
      , poleman
      , poltergeist
      , possessedOathspeaker
      , preyingByakhee
      , priestOfAThousandMasks
      , priestOfDagon
      , priestessOfTheCoven
      , primordialEvil
      , professorWilliamDyerProfessorOfGeology
      , protoplasmicMass
      , pursuingMotorcar
      , rampagingShoggoth
      , ravagerFromTheDeep
      , ravenousGhoul
      , reawakenedElderThing
      , relentlessDarkYoung
      , riftSeeker
      , roachSwarm
      , roaldEllsworthIntrepidExplorer
      , robertFriendlyDisgruntledDockworker
      , royalEmissary
      , rookieCop
      , ruthTurner
      , salvatoreNeri
      , savageShantak
      , theBloodlessMan
      , theBloodlessManUnleashed
      , savioCorvi
      , scholarFromYith
      , scientistOfYith
      , screechingByakhee
      , sebastienMoreau
      , seekerOfCarcosa
      , seepingNightmare
      , serpentFromYoth
      , serpentGuardian
      , serpentOfTenochtitlan
      , servantOfManyMouths
      , servantOfTheLurker
      , sethBishop
      , sethBishopThrallOfYogSothoth
      , shadowHound
      , silasBishop
      , skitteringNonsense
      , slimeCoveredDhole
      , slitheringDhole
      , spawnOfHali
      , specterOfDeath
      , spectralRaven
      , spiderOfLeng
      , stalkingManticore
      , stealthyByakhee
      , stealthyZoog
      , stolenMind
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
      , theCrawlingMist
      , theConductorBeastFromBeyondTheGate
      , theExperiment
      , theMaskedHunter
      , theNamelessMadness
      , theOrganistDrapedInMystery
      , theOrganistHopelessIDefiedHim
      , theSpectralWatcher
      , theTerrorOfDevilReef_164
      , theTerrorOfDevilReef_165
      , theTerrorOfDevilReefRelentlessMonstrosity
      , theRougarou
      , theUnnamable
      , theWingedSerpent
      , theWingedSerpentTheFuryOfYig
      , thrall
      , tidalTerror
      , tindalosAlpha
      , umordhoth
      , unsealedPhantasm
      , valentinoRivas
      , vassalOfTheLurker
      , vengefulSerpent
      , vengefulSpecter
      , vengefulWitch
      , victoriaDevereux
      , webSpinner
      , whippoorwill
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
      ]

allSpecialEnemyCards :: Map CardCode CardDef
allSpecialEnemyCards =
  mapFromList $ map (toCardCode &&& id) [flyingPolyp, reanimatedDead, nyarlathotepTrueShape]

mobEnforcer :: CardDef
mobEnforcer =
  (basicWeakness "01101" "Mob Enforcer")
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01601"]
    }

silverTwilightAcolyte :: CardDef
silverTwilightAcolyte =
  (basicWeakness "01102" "Silver Twilight Acolyte")
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01602"]
    }

stubbornDetective :: CardDef
stubbornDetective =
  (basicWeakness "01103" "Stubborn Detective")
    { cdCardTraits = setFromList [Humanoid, Detective]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdAlternateCardCodes = ["01603"]
    }

ghoulPriest :: CardDef
ghoulPriest =
  (enemy "01116" "Ghoul Priest" TheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

fleshEater :: CardDef
fleshEater =
  (enemy "01118" "Flesh-Eater" TheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdVictoryPoints = Just 1
    }

icyGhoul :: CardDef
icyGhoul =
  (enemy "01119" "Icy Ghoul" TheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdVictoryPoints = Just 1
    }

theMaskedHunter :: CardDef
theMaskedHunter =
  unique
    $ ( enemy
          "01121b"
          ("The Masked Hunter" <:> "Silently Stalking")
          TheMidnightMasks
          1
      )
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }

wolfManDrew :: CardDef
wolfManDrew =
  unique
    $ (enemy "01137" ("\"Wolf-Man\" Drew" <:> "The Cannibal") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

hermanCollins :: CardDef
hermanCollins =
  unique
    $ (enemy "01138" ("Herman Collins" <:> "The Undertaker") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

peterWarren :: CardDef
peterWarren =
  unique
    $ (enemy "01139" ("Peter Warren" <:> "The Occult Professor") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

victoriaDevereux :: CardDef
victoriaDevereux =
  unique
    $ (enemy "01140" ("Victoria Devereux" <:> "The Collector") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

ruthTurner :: CardDef
ruthTurner =
  unique
    $ (enemy "01141" ("Ruth Turner" <:> "The Mortician") CultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

umordhoth :: CardDef
umordhoth =
  unique
    $ (enemy "01157" ("Umôrdhoth" <:> "The Devourer Below") TheDevourerBelow 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

swarmOfRats :: CardDef
swarmOfRats =
  (enemy "01159" "Swarm of Rats" Rats 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

ghoulMinion :: CardDef
ghoulMinion =
  (enemy "01160" "Ghoul Minion" Ghouls 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

ravenousGhoul :: CardDef
ravenousGhoul =
  (enemy "01161" "Ravenous Ghoul" Ghouls 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

acolyte :: CardDef
acolyte =
  (enemy "01169" "Acolyte" DarkCult 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

wizardOfTheOrder :: CardDef
wizardOfTheOrder =
  (enemy "01170" "Wizard of the Order" DarkCult 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

huntingNightgaunt :: CardDef
huntingNightgaunt =
  (enemy "01172" "Hunting Nightgaunt" Nightgaunts 2)
    { cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

screechingByakhee :: CardDef
screechingByakhee =
  (enemy "01175" "Screeching Byakhee" AgentsOfHastur 2)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

yithianObserver :: CardDef
yithianObserver =
  (enemy "01177" "Yithian Observer" AgentsOfYogSothoth 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdVictoryPoints = Just 1
    }

relentlessDarkYoung :: CardDef
relentlessDarkYoung =
  (enemy "01179" "Relentless Dark Young" AgentsOfShubNiggurath 1)
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

goatSpawn :: CardDef
goatSpawn =
  (enemy "01180" "Goat Spawn" AgentsOfShubNiggurath 3)
    { cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

youngDeepOne :: CardDef
youngDeepOne =
  (enemy "01181" "Young Deep One" AgentsOfCthulhu 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theExperiment :: CardDef
theExperiment =
  unique
    $ ( enemy
          "02058"
          ("The Experiment" <:> "Something Went Terribly Wrong")
          ExtracurricularActivity
          1
      )
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      , cdVictoryPoints = Just 2
      }

cloverClubPitBoss :: CardDef
cloverClubPitBoss =
  (enemy "02078" "Clover Club Pit Boss" TheHouseAlwaysWins 1)
    { cdCardTraits = setFromList [Humanoid, Criminal, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

thrall :: CardDef
thrall =
  (enemy "02086" "Thrall" BishopsThralls 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

wizardOfYogSothoth :: CardDef
wizardOfYogSothoth =
  (enemy "02087" "Wizard of Yog-Sothoth" BishopsThralls 1)
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

whippoorwill :: CardDef
whippoorwill =
  (enemy "02090" "Whippoorwill" Whippoorwills 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

avianThrall :: CardDef
avianThrall =
  (enemy "02094" "Avian Thrall" BeastThralls 2)
    { cdCardTraits = setFromList [Creature, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

lupineThrall :: CardDef
lupineThrall =
  (enemy "02095" "Lupine Thrall" BeastThralls 2)
    { cdCardTraits = setFromList [Creature, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

oBannionsThug :: CardDef
oBannionsThug =
  (enemy "02097" "O'Bannion's Thug" NaomisCrew 2)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }

mobster :: CardDef
mobster =
  (enemy "02098" "Mobster" NaomisCrew 2)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

conglomerationOfSpheres :: CardDef
conglomerationOfSpheres =
  (enemy "02103" "Conglomeration of Spheres" HideousAbominations 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

servantOfTheLurker :: CardDef
servantOfTheLurker =
  (enemy "02104" "Servant of the Lurker" HideousAbominations 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

huntingHorror :: CardDef
huntingHorror =
  ( enemy
      "02141"
      ("Hunting Horror" <:> "Spawned from the Void")
      TheMiskatonicMuseum
      1
  )
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

grapplingHorror :: CardDef
grapplingHorror =
  (enemy "02182" "Grappling Horror" TheEssexCountyExpress 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

emergentMonstrosity :: CardDef
emergentMonstrosity =
  (enemy "02183" "Emergent Monstrosity" TheEssexCountyExpress 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdVictoryPoints = Just 1
    }

silasBishop :: CardDef
silasBishop =
  unique
    $ (enemy "02216" ("Silas Bishop" <:> "Infused With Evil") BloodOnTheAltar 1)
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = singleton Keyword.Massive
      , cdVictoryPoints = Just 2
      }

servantOfManyMouths :: CardDef
servantOfManyMouths =
  (enemy "02224" "Servant of Many Mouths" BloodOnTheAltar 3)
    { cdCardTraits = singleton Humanoid
    , cdKeywords = singleton Keyword.Retaliate
    }

broodOfYogSothoth :: CardDef
broodOfYogSothoth =
  (enemy "02255" "Brood of Yog-Sothoth" UndimensionedAndUnseen 5)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

sethBishop :: CardDef
sethBishop =
  unique
    $ (enemy "02293" ("Seth Bishop" <:> "Sorcerer of Dunwich") WhereDoomAwaits 1)
      { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

devoteeOfTheKey :: CardDef
devoteeOfTheKey =
  (enemy "02294" "Devotee of the Key" WhereDoomAwaits 2)
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    }

crazedShoggoth :: CardDef
crazedShoggoth =
  (enemy "02295" "Crazed Shoggoth" WhereDoomAwaits 1)
    { cdCardTraits = setFromList [Monster, Shoggoth]
    , cdVictoryPoints = Just 1
    }

yogSothoth :: CardDef
yogSothoth =
  unique
    $ ( enemy
          "02323"
          ("Yog-Sothoth" <:> "The Lurker Beyond the Threshold")
          LostInTimeAndSpace
          1
      )
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords =
          setFromList
            [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
      }

interstellarTraveler :: CardDef
interstellarTraveler =
  (enemy "02329" "Interstellar Traveler" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

yithianStarseeker :: CardDef
yithianStarseeker =
  (enemy "02330" "Yithian Starseeker" LostInTimeAndSpace 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

graveyardGhouls :: CardDef
graveyardGhouls =
  (weakness "03017" "Graveyard Ghouls")
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theThingThatFollows :: CardDef
theThingThatFollows =
  unique
    $ (basicWeakness "03042" "The Thing That Follows")
      { cdCardTraits = setFromList [Monster, Curse]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

theManInThePallidMask :: CardDef
theManInThePallidMask =
  unique
    $ (weakness "03059" "The Man in the Pallid Mask")
      { cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdEncounterSet = Just CurtainCall
      , cdEncounterSetQuantity = Just 1
      }

royalEmissary :: CardDef
royalEmissary =
  unique
    $ (enemy "03060" ("Royal Emissary" <:> "Messenger from Aldebaran") CurtainCall 1)
      { cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords =
          setFromList
            [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

constanceDumaine :: CardDef
constanceDumaine =
  unique
    $ ( enemy
          "03065b"
          ("Constance Dumaine" <:> "A Little Too Sociable")
          TheLastKing
          1
      )
      { cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

jordanPerry :: CardDef
jordanPerry =
  unique
    $ (enemy "03066b" ("Jordan Perry" <:> "An Imposing Presence") TheLastKing 1)
      { cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

ishimaruHaruko :: CardDef
ishimaruHaruko =
  unique
    $ (enemy "03067b" ("Ishimaru Haruko" <:> "Just Skin and Bones") TheLastKing 1)
      { cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

sebastienMoreau :: CardDef
sebastienMoreau =
  unique
    $ (enemy "03068b" ("Sebastien Moreau" <:> "Savage Hysteria") TheLastKing 1)
      { cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

ashleighClarke :: CardDef
ashleighClarke =
  unique
    $ (enemy "03069b" ("Ashleigh Clarke" <:> "Songs Die Unheard") TheLastKing 1)
      { cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

dianneDevine :: CardDef
dianneDevine =
  unique
    $ (enemy "03081" ("Dianne Devine" <:> "Mercurial and Mischevious") TheLastKing 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = singleton Keyword.Aloof
      }

swiftByakhee :: CardDef
swiftByakhee =
  (enemy "03086" "Swift Byakhee" EncounterSet.Byakhee 2)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

beastOfAldebaran :: CardDef
beastOfAldebaran =
  (enemy "03088" "Beast of Aldebaran" InhabitantsOfCarcosa 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

spawnOfHali :: CardDef
spawnOfHali =
  (enemy "03089" "Spawn of Hali" InhabitantsOfCarcosa 2)
    { cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Retaliate
    }

poltergeist :: CardDef
poltergeist =
  (enemy "03093" "Poltergeist" Hauntings 2)
    { cdCardTraits = setFromList [Monster, Geist]
    }

maniac :: CardDef
maniac =
  (enemy "03095" "Seer of the Sign" HastursGift 2)
    { cdCardTraits = setFromList [Humanoid, Lunatic]
    }

youngPsychopath :: CardDef
youngPsychopath =
  (enemy "03096" "Puppet of Hastur" HastursGift 2)
    { cdCardTraits = setFromList [Humanoid, Lunatic]
    }

fanatic :: CardDef
fanatic =
  (enemy "03098" "Fanatic" CultOfTheYellowSign 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

agentOfTheKing :: CardDef
agentOfTheKing =
  (enemy "03099" "Agent of the King" CultOfTheYellowSign 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

roachSwarm :: CardDef
roachSwarm =
  (enemy "03103" "Roach Swarm" DecayAndFilth 2)
    { cdCardTraits = singleton Creature
    }

possessedOathspeaker :: CardDef
possessedOathspeaker =
  ( enemy
      "03140"
      ("Possessed Oathspeaker" <:> "A Damnable Fate")
      EchoesOfThePast
      1
  )
    { cdCardTraits = setFromList [Monster, Servitor, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

seekerOfCarcosa :: CardDef
seekerOfCarcosa =
  (enemy "03144" "Seeker of Carcosa" EchoesOfThePast 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Aloof
    }

danielChesterfield :: CardDef
danielChesterfield =
  unique
    $ ( enemy
          "03182b"
          ("Daniel Chesterfield" <:> "…Or At Least, What's Left of Him")
          TheUnspeakableOath
          1
      )
      { cdCardTraits = setFromList [Humanoid, Lunatic, Elite]
      , cdVictoryPoints = Just 1
      , cdDoubleSided = True
      , cdOtherSide = Just "03182a"
      }

asylumGorger :: CardDef
asylumGorger =
  (enemy "03183" "Asylum Gorger" TheUnspeakableOath 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Hunter
    }

madPatient :: CardDef
madPatient =
  (enemy "03184" "Haunted Patient" TheUnspeakableOath 3)
    { cdCardTraits = setFromList [Humanoid, Lunatic]
    }

theOrganistHopelessIDefiedHim :: CardDef
theOrganistHopelessIDefiedHim =
  unique
    $ ( enemy
          "03221a"
          ("The Organist" <:> "Hopeless, I Defied Him")
          APhantomOfTruth
          1
      )
      { cdCardTraits = setFromList [Humanoid, Avatar, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdDoubleSided = True
      , cdOtherSide = Just "03221b"
      }

theOrganistDrapedInMystery :: CardDef
theOrganistDrapedInMystery =
  unique
    $ (enemy "03221b" ("The Organist" <:> "Draped in Mystery") APhantomOfTruth 1)
      { cdCardTraits = setFromList [Humanoid, Avatar, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdDoubleSided = True
      , cdOtherSide = Just "03221a"
      }

stealthyByakhee :: CardDef
stealthyByakhee =
  (enemy "03222" "Stealthy Byakhee" APhantomOfTruth 2)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = singleton Keyword.Hunter
    }

specterOfDeath :: CardDef
specterOfDeath =
  (enemy "03241b" ("Specter of Death" <:> "A Force From Beyond") ThePallidMask 1)
    { cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

catacombsDocent :: CardDef
catacombsDocent =
  (enemy "03258" "Catacombs Docent" ThePallidMask 3)
    { cdCardTraits = setFromList [Humanoid, Lunatic]
    }

corpseDweller :: CardDef
corpseDweller =
  (enemy "03259" "Corpse Dweller" ThePallidMask 3)
    { cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

tidalTerror :: CardDef
tidalTerror =
  (enemy "03300" "Tidal Terror" BlackStarsRise 2)
    { cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    }

riftSeeker :: CardDef
riftSeeker =
  (enemy "03301" "Rift Seeker" BlackStarsRise 2)
    { cdCardTraits = setFromList [Monster, Byakhee, Cultist]
    }

hasturTheKingInYellow :: CardDef
hasturTheKingInYellow =
  unique
    $ (enemy "03332" ("Hastur" <:> "The King in Yellow") DimCarcosa 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      }

hasturLordOfCarcosa :: CardDef
hasturLordOfCarcosa =
  unique
    $ (enemy "03333" ("Hastur" <:> "Lord of Carcosa") DimCarcosa 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

hasturTheTatteredKing :: CardDef
hasturTheTatteredKing =
  unique
    $ (enemy "03334" ("Hastur" <:> "The Tattered King") DimCarcosa 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }

creatureOutOfDemhe :: CardDef
creatureOutOfDemhe =
  (enemy "03335" "Creature Out of Demhe" DimCarcosa 1)
    { cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Massive
    }

wingedOne :: CardDef
wingedOne =
  (enemy "03336" "Winged One" DimCarcosa 1)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = singleton Keyword.Retaliate
    }

serpentsOfYig :: CardDef
serpentsOfYig =
  (weakness "04014" "Serpents of Yig")
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdRevelation = IsRevelation
    }

ichtaca :: CardDef
ichtaca =
  unique
    $ (enemy "04052" ("Ichtaca" <:> "Keeper of the Eztli") TheUntamedWilds 1)
      { cdCardTraits = setFromList [Humanoid, Eztli, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

harbingerOfValusia :: CardDef
harbingerOfValusia =
  unique
    $ ( enemy
          "04062"
          ("Harbinger of Valusia" <:> "The Sleeper Awakens")
          TheDoomOfEztli
          1
      )
      { cdCardTraits = setFromList [Humanoid, Serpent, Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
      , cdVengeancePoints = Just 5
      }

pitViper :: CardDef
pitViper =
  (enemy "04078" "Pit Viper" Serpents 3)
    { cdCardTraits = setFromList [Creature, Serpent]
    , cdVengeancePoints = Just 1
    }

boaConstrictor :: CardDef
boaConstrictor =
  (enemy "04079" "Boa Constrictor" Serpents 1)
    { cdCardTraits = setFromList [Creature, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdVengeancePoints = Just 2
    }

broodOfYig :: CardDef
broodOfYig =
  (enemy "04083" "Brood of Yig" AgentsOfYig 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    }

serpentFromYoth :: CardDef
serpentFromYoth =
  (enemy "04084" "Serpent from Yoth" AgentsOfYig 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVictoryPoints = Just 1
    }

eztliGuardian :: CardDef
eztliGuardian =
  (enemy "04086" "Eztli Guardian" GuardiansOfTime 2)
    { cdCardTraits = setFromList [Humanoid, Eztli]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
    }

brotherhoodCultist :: CardDef
brotherhoodCultist =
  (enemy "04095" "Brotherhood Cultist" PnakoticBrotherhood 2)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    }

fangOfYig :: CardDef
fangOfYig =
  (enemy "04098" "Fang of Yig" YigsVenom 2)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = singleton Keyword.Retaliate
    }

harlanEarnstoneCrazedByTheCurse :: CardDef
harlanEarnstoneCrazedByTheCurse =
  unique
    $ (enemy "04122b" ("Harlan Earnstone" <:> "Crazed by the Curse") ThreadsOfFate 1)
      { cdCardTraits = setFromList [Humanoid, Cursed, Elite]
      , cdVictoryPoints = Just 1
      }

henryDeveauAlejandrosKidnapper :: CardDef
henryDeveauAlejandrosKidnapper =
  unique
    $ (enemy "04130b" ("Henry Deveau" <:> "Alejandro's Kidnapper") ThreadsOfFate 1)
      { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
      , cdVictoryPoints = Just 1
      }

mariaDeSilvaKnowsMoreThanSheLetsOn :: CardDef
mariaDeSilvaKnowsMoreThanSheLetsOn =
  unique
    $ (enemy "04137b" ("Maria DeSilva" <:> "Knows More Than She Lets On") ThreadsOfFate 1)
      { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
      , cdVictoryPoints = Just 1
      , cdKeywords = singleton Keyword.Retaliate
      }

padmaAmrita :: CardDef
padmaAmrita =
  unique
    $ (enemy "04186" ("Padma Amrita" <:> "Cold-Blooded Charmer") TheBoundaryBeyond 1)
      { cdCardTraits = setFromList [Humanoid, Serpent, Servitor, Elite]
      , cdVictoryPoints = Just 2
      , cdVengeancePoints = Just 2
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate, Keyword.Hunter]
      }

serpentOfTenochtitlan :: CardDef
serpentOfTenochtitlan =
  (enemy "04187" "Serpent of Tenochtitlán" TheBoundaryBeyond 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVictoryPoints = Just 1
    , cdVengeancePoints = Just 1
    }

handOfTheBrotherhood :: CardDef
handOfTheBrotherhood =
  (enemy "04188" "Hand of the Brotherhood" TheBoundaryBeyond 2)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

theWingedSerpent :: CardDef
theWingedSerpent =
  (enemy "04209b" ("The Winged Serpent" <:> "The Wrath of Yig") PillarsOfJudgement 1)
    { cdCardTraits = setFromList [Monster, Serpent, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive]
    }

apexStrangleweed :: CardDef
apexStrangleweed =
  (enemy "04219" "Apex Strangleweed" PillarsOfJudgement 2)
    { cdCardTraits = setFromList [Creature, Flora]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

basilisk :: CardDef
basilisk =
  (enemy "04220" "Basilisk" PillarsOfJudgement 2)
    { cdCardTraits = setFromList [Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdVengeancePoints = Just 2
    }

keeperOfTheGreatLibrary :: CardDef
keeperOfTheGreatLibrary =
  (enemy "04257" "Keeper of the Great Library" TheCityOfArchives 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
    }

scientistOfYith :: CardDef
scientistOfYith =
  (enemy "04258" "Scientist of Yith" TheCityOfArchives 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = singleton Keyword.Aloof
    }

scholarFromYith :: CardDef
scholarFromYith =
  (enemy "04259" "Scholar from Yith" TheCityOfArchives 3)
    { cdCardTraits = setFromList [Monster, Yithian]
    }

yig :: CardDef
yig =
  unique
    $ (enemy "04296" ("Yig" <:> "The Father of Serpents") TheDepthsOfYoth 1)
      { cdCardTraits = setFromList [AncientOne, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 5
      }

pitWarden :: CardDef
pitWarden =
  (enemy "04297" "Pit Warden" TheDepthsOfYoth 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVengeancePoints = Just 1
    }

eaterOfTheDepths :: CardDef
eaterOfTheDepths =
  (enemy "04298" "Eater of the Depths" TheDepthsOfYoth 1)
    { cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 2
    }

ichtacaScionOfYig :: CardDef
ichtacaScionOfYig =
  unique
    $ (enemy "04325" ("Ichtaca" <:> "Scion of Yig") ShatteredAeons 1)
      { cdCardTraits = setFromList [Humanoid, Monster, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

alejandroVela :: CardDef
alejandroVela =
  unique
    $ (enemy "04326" ("Alejandro Vela" <:> "Or, Is He?") ShatteredAeons 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }

formlessSpawn :: CardDef
formlessSpawn =
  (enemy "04337" ("Formless Spawn" <:> "From the Abyss") ShatteredAeons 1)
    { cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 2
    }

temporalDevourer :: CardDef
temporalDevourer =
  (enemy "04338" "Temporal Devourer" ShatteredAeons 2)
    { cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = singleton Keyword.Hunter
    }

hoods :: CardDef
hoods =
  (weakness "05017" "Hoods")
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

anetteMason :: CardDef
anetteMason =
  unique
    $ (enemy "05057" ("Anette Mason" <:> "The High Priestess") TheWitchingHour 1)
      { cdCardTraits = setFromList [Humanoid, Witch, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdVictoryPoints = Just 2
      }

josefMeiger :: CardDef
josefMeiger =
  unique
    $ (enemy "05085" ("Josef Meiger" <:> "Lodge Host") AtDeathsDoorstep 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdDoubleSided = True
      , cdVictoryPoints = Just 2
      }

theSpectralWatcher :: CardDef
theSpectralWatcher =
  unique
    $ (enemy "05086" ("The Spectral Watcher" <:> "You Are Its Prey") TheWatcher 1)
      { cdCardTraits = setFromList [AncientOne, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      }

piperOfAzathoth :: CardDef
piperOfAzathoth =
  (enemy "05088" "Piper of Azathoth" AgentsOfAzathoth 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

covenInitiate :: CardDef
covenInitiate =
  (enemy "05090" "Coven Initiate" AnettesCoven 3)
    { cdCardTraits = setFromList [Humanoid, Witch]
    , cdRevelation = IsRevelation
    }

priestessOfTheCoven :: CardDef
priestessOfTheCoven =
  (enemy "05091" "Priestess of the Coven" AnettesCoven 1)
    { cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = singleton Keyword.Retaliate
    }

lodgeNeophyte :: CardDef
lodgeNeophyte =
  (enemy "05095" "Lodge Neophyte" SilverTwilightLodge 3)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }

keeperOfSecrets :: CardDef
keeperOfSecrets =
  (enemy "05096" "Keeper of Secrets" SilverTwilightLodge 1)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }

netherMist :: CardDef
netherMist =
  (enemy "05100" "Nether Mist" SpectralPredators 1)
    { cdCardTraits = setFromList [Monster, Spectral]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

shadowHound :: CardDef
shadowHound =
  (enemy "05101" "Shadow Hound" SpectralPredators 2)
    { cdCardTraits = setFromList [Monster, Spectral]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

wraith :: CardDef
wraith =
  (enemy "05103" "Wraith" TrappedSpirits 2)
    { cdCardTraits = setFromList [Monster, Geist, Spectral]
    , cdKeywords = singleton Keyword.Hunter
    }

brownJenkin :: CardDef
brownJenkin =
  unique
    $ (enemy "05148" ("Brown Jenkin" <:> "The Witch's Familiar") TheSecretName 1)
      { cdCardTraits = setFromList [Creature, Familiar, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      }

nahab :: CardDef
nahab =
  unique
    $ (enemy "05149" ("Nahab" <:> "She Who Signed the Black Book") TheSecretName 1)
      { cdCardTraits = setFromList [Monster, Geist, Witch, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      }

heretic_A :: CardDef
heretic_A =
  (enemy "05178a" "Heretic" TheWagesOfSin 1)
    { cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
    }

heretic_C :: CardDef
heretic_C =
  (enemy "05178c" "Heretic" TheWagesOfSin 1)
    { cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
    }

heretic_E :: CardDef
heretic_E =
  (enemy "05178e" "Heretic" TheWagesOfSin 1)
    { cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
    }

heretic_G :: CardDef
heretic_G =
  (enemy "05178g" "Heretic" TheWagesOfSin 1)
    { cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
    }

heretic_I :: CardDef
heretic_I =
  (enemy "05178i" "Heretic" TheWagesOfSin 1)
    { cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
    }

heretic_K :: CardDef
heretic_K =
  (enemy "05178k" "Heretic" TheWagesOfSin 1)
    { cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
    }

vengefulWitch :: CardDef
vengefulWitch =
  (enemy "05179" "Vengeful Witch" TheWagesOfSin 2)
    { cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

malevolentSpirit :: CardDef
malevolentSpirit =
  (enemy "05180" "Malevolent Spirit" TheWagesOfSin 2)
    { cdCardTraits = setFromList [Monster, Geist, Spectral]
    }

nathanWickMasterOfInitiation :: CardDef
nathanWickMasterOfInitiation =
  unique
    $ (enemy "05217a" ("Nathan Wick" <:> "Master of Initiation") ForTheGreaterGood 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdDoubleSided = True
      , cdVictoryPoints = Just 1
      , cdOtherSide = Just "05217b"
      }

nathanWickMasterOfIndoctrination :: CardDef
nathanWickMasterOfIndoctrination =
  unique
    $ (enemy "05217b" ("Nathan Wick" <:> "Master of Indoctrination") ForTheGreaterGood 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Alert
      , cdDoubleSided = True
      , cdVictoryPoints = Just 1
      , cdOtherSide = Just "05217a"
      }

lodgeJailor :: CardDef
lodgeJailor =
  (enemy "05218" "Lodge Jailor" ForTheGreaterGood 1)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }

cellKeeper :: CardDef
cellKeeper =
  (enemy "05219" "Cell Keeper" ForTheGreaterGood 1)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Alert
    }

summonedBeast :: CardDef
summonedBeast =
  (enemy "05220" ("Summoned Beast" <:> "Guardian of the Trap") ForTheGreaterGood 1)
    { cdCardTraits = setFromList [Monster, SilverTwilight, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    }

knightOfTheInnerCircle :: CardDef
knightOfTheInnerCircle =
  (enemy "05221" "Knight of the Inner Circle" ForTheGreaterGood 2)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof, Keyword.Hunter]
    }

knightOfTheOuterVoid :: CardDef
knightOfTheOuterVoid =
  (enemy "05222" "Knight of the Outer Void" ForTheGreaterGood 2)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Peril, Keyword.Retaliate]
    , cdRevelation = IsRevelation
    }

gavriellaMizrah :: CardDef
gavriellaMizrah =
  unique
    $ (enemy "05262b" ("Gavriella Mizrah" <:> "You're Next") UnionAndDisillusion 1)
      { cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

jeromeDavids :: CardDef
jeromeDavids =
  unique
    $ (enemy "05263b" ("Jerome Davids" <:> "Starved for Answers") UnionAndDisillusion 1)
      { cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

pennyWhite :: CardDef
pennyWhite =
  unique
    $ (enemy "05264b" ("Penny White" <:> "Tragic Loss") UnionAndDisillusion 1)
      { cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

valentinoRivas :: CardDef
valentinoRivas =
  unique
    $ (enemy "05265b" ("Valentino Rivas" <:> "Ripped Asunder") UnionAndDisillusion 1)
      { cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdDoubleSided = True
      , cdVictoryPoints = Just 0
      }

whippoorwillUnionAndDisillusion :: CardDef
whippoorwillUnionAndDisillusion =
  (enemy "05266" "Whippoorwill" UnionAndDisillusion 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

spectralRaven :: CardDef
spectralRaven =
  (enemy "05267" "Spectral Raven" UnionAndDisillusion 2)
    { cdCardTraits = setFromList [Creature, Spectral]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    }

anetteMasonReincarnatedEvil :: CardDef
anetteMasonReincarnatedEvil =
  (enemy "05286b" ("Anette Mason" <:> "Reincarnated Evil") MusicOfTheDamned 1)
    { cdCardTraits = setFromList [Humanoid, Witch, Servitor, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

carlSanfordDeathlessFanatic :: CardDef
carlSanfordDeathlessFanatic =
  (enemy "05288b" ("Carl Sanford" <:> "Deathless Fanatic") SecretsOfTheUniverse 1)
    { cdCardTraits = setFromList [Humanoid, SilverTwilight, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

lodgeEnforcer :: CardDef
lodgeEnforcer =
  (enemy "05309" "Lodge Enforcer" SecretsOfTheUniverse 2)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Retaliate
    , cdVictoryPoints = Just 1
    }

witnessOfChaos :: CardDef
witnessOfChaos =
  (enemy "05311" "Witness of Chaos" MusicOfTheDamned 2)
    { cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

mindlessDancer :: CardDef
mindlessDancer =
  (enemy "05341" "Mindless Dancer" BeforeTheBlackThrone 3)
    { cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    }

azathoth :: CardDef
azathoth =
  (enemy "05346" ("Azathoth" <:> "The Primal Chaos") BeforeTheBlackThrone 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdUnique = True
    }

tonysQuarry :: CardDef
tonysQuarry =
  (weakness "06012" "Tony's Quarry")
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

watcherFromAnotherDimension :: CardDef
watcherFromAnotherDimension =
  unique
    $ (weakness "06017" "Watcher from Another Dimension")
      { cdCardTraits = setFromList [Monster, Extradimensional]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdRevelation = IsRevelation
      }

guardianOfTheCrystallizer :: CardDef
guardianOfTheCrystallizer =
  (weakness "06025" "Guardian of the Crystallizer")
    { cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Bonded 1 "06024", Keyword.Hunter]
    }

yourWorstNightmare :: CardDef
yourWorstNightmare =
  (basicWeakness "06038" "Your Worst Nightmare")
    { cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    , cdDeckRestrictions = [MultiplayerOnly]
    }

kamanThah :: CardDef
kamanThah =
  (enemy "06057" ("Kaman-Thah" <:> "Priest of the Dreamlands") BeyondTheGatesOfSleep 1)
    { cdCardTraits = setFromList [Dreamlands, Warden, Elite]
    , cdUnique = True
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    , cdDoubleSided = True
    }

nasht :: CardDef
nasht =
  (enemy "06058" ("Nasht" <:> "Priest of the Dreamlands") BeyondTheGatesOfSleep 1)
    { cdCardTraits = setFromList [Dreamlands, Warden, Elite]
    , cdUnique = True
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    , cdDoubleSided = True
    }

laboringGug :: CardDef
laboringGug =
  (enemy "06060" "Laboring Gug" BeyondTheGatesOfSleep 1)
    { cdCardTraits = setFromList [Monster, Gug]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

ancientZoog :: CardDef
ancientZoog =
  (enemy "06061" "Ancient Zoog" BeyondTheGatesOfSleep 1)
    { cdCardTraits = setFromList [Creature, Zoog, Elite]
    , cdKeywords = singleton Keyword.Aloof
    }

suspiciousOrderly :: CardDef
suspiciousOrderly =
  (enemy "06081" "Suspicious Orderly" WakingNightmare 2)
    { cdCardTraits = setFromList [Humanoid, Staff]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

corruptedOrderly :: CardDef
corruptedOrderly =
  (enemy "06082" "Corrupted Orderly" WakingNightmare 2)
    { cdCardTraits = setFromList [Humanoid, Staff, Spider]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

greyWeaver :: CardDef
greyWeaver =
  (enemy "06084" "Grey Weaver" AgentsOfAtlachNacha 2)
    { cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

theCrawlingMist :: CardDef
theCrawlingMist =
  (enemy "06086" "The Crawling Mist" AgentsOfNyarlathotep 1)
    { cdCardTraits = setFromList [Monster, Avatar]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

huntingGhast :: CardDef
huntingGhast =
  (enemy "06091" "Hunting Ghast" CreaturesOfTheUnderworld 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghast]
    , cdKeywords = singleton Keyword.Hunter
    }

lumberingGug :: CardDef
lumberingGug =
  (enemy "06092" "Lumbering Gug" CreaturesOfTheUnderworld 1)
    { cdCardTraits = setFromList [Monster, Gug]
    }

spiderOfLeng :: CardDef
spiderOfLeng =
  (enemy "06101" "Spider of Leng" Spiders 1)
    { cdCardTraits = setFromList [Monster, Spider]
    }

swarmOfSpiders :: CardDef
swarmOfSpiders =
  (enemy "06102" "Swarm of Spiders" Spiders 3)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdKeywords = setFromList [Keyword.Swarming (Static 2)]
    }

corsairOfLeng :: CardDef
corsairOfLeng =
  (enemy "06105" "Corsair of Leng" Corsairs 2)
    { cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = singleton Keyword.Alert
    }

furtiveZoog :: CardDef
furtiveZoog =
  (enemy "06106" "Furtive Zoog" Zoogs 2)
    { cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Swarming (Static 1)]
    }

stealthyZoog :: CardDef
stealthyZoog =
  (enemy "06107" "Stealthy Zoog" Zoogs 2)
    { cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Swarming (Static 1)]
    }

inconspicuousZoog :: CardDef
inconspicuousZoog =
  (enemy "06108" "Inconspicuous Zoog" Zoogs 1)
    { cdCardTraits = setFromList [Creature, Zoog]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 2)]
    }

catsOfUlthar :: CardDef
catsOfUlthar =
  (enemy "06145" "Cats of Ulthar" TheSearchForKadath 1)
    { cdCardTraits = setFromList [Creature, Elite]
    , cdKeywords = singleton $ Keyword.Swarming (Static 2)
    , cdVictoryPoints = Just 1
    }

stalkingManticore :: CardDef
stalkingManticore =
  (enemy "06146" "Stalking Manticore" TheSearchForKadath 1)
    { cdCardTraits = setFromList [Creature, Monster, Elite]
    , cdVictoryPoints = Just 1
    }

hordeOfNight :: CardDef
hordeOfNight =
  (enemy "06147" "Horde of Night" TheSearchForKadath 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdVictoryPoints = Just 1
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Swarming (Static 1)]
    }

beingsOfIb :: CardDef
beingsOfIb =
  (enemy "06148" "Beings of Ib" TheSearchForKadath 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdVictoryPoints = Just 1
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter, Keyword.Swarming (PerPlayer 1)]
    }

priestOfAThousandMasks :: CardDef
priestOfAThousandMasks =
  (enemy "06149" "Priest of a Thousand Masks" TheSearchForKadath 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

tenebrousNightgaunt :: CardDef
tenebrousNightgaunt =
  (enemy "06150" "Tenebrous Nightgaunt" TheSearchForKadath 2)
    { cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = singleton Keyword.Hunter
    }

packOfVooniths :: CardDef
packOfVooniths =
  (enemy "06151" "Pack of Vooniths" TheSearchForKadath 2)
    { cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton (Keyword.Swarming (Static 1))
    }

nightriders :: CardDef
nightriders =
  (enemy "06152" "Nightriders" TheSearchForKadath 2)
    { cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton (Keyword.Swarming (Static 1))
    }

theUnnamable :: CardDef
theUnnamable =
  unique
    $ (enemy "06169b" ("The Unnamable" <:> "The Ultimate Abomination") AThousandShapesOfHorror 1)
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      }

moonLizard :: CardDef
moonLizard =
  (enemy "06226" "Moon Lizard" DarkSideOfTheMoon 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 2
    }

moonboundByakhee :: CardDef
moonboundByakhee =
  (enemy "06227" "Moonbound Byakhee" DarkSideOfTheMoon 2)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

catsFromSaturn :: CardDef
catsFromSaturn =
  (enemy "06228" "Cats from Saturn" DarkSideOfTheMoon 3)
    { cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 0)]
    }

moonBeast :: CardDef
moonBeast =
  (enemy "06229" "Moon-Beast" DarkSideOfTheMoon 2)
    { cdCardTraits = setFromList [Monster, Servitor]
    , cdKeywords = singleton Keyword.Retaliate
    , cdVictoryPoints = Just 1
    }

gugSentinel :: CardDef
gugSentinel =
  (enemy "06267" "Gug Sentinel" PointOfNoReturn 1)
    { cdCardTraits = setFromList [Monster, Gug]
    , cdVictoryPoints = Just 1
    }

slitheringDhole :: CardDef
slitheringDhole =
  (enemy "06271" "Slithering Dhole" TerrorOfTheVale 1)
    { cdCardTraits = setFromList [Monster, Dhole, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

pitchSpider :: CardDef
pitchSpider =
  (enemy "06273" "Pitch Spider" TerrorOfTheVale 2)
    { cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = setFromList [Keyword.Swarming (Static 0)]
    }

unboundBeast :: CardDef
unboundBeast =
  (weakness "06283" "Unbound Beast")
    { cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdRevelation = IsRevelation
    }

nyarlathotepTheCrawlingChaos :: CardDef
nyarlathotepTheCrawlingChaos =
  unique
    $ (enemy "06306" ("Nyarlathotep" <:> "The Crawling Chaos") WhereTheGodsDwell 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepTheFacelessWhisperer :: CardDef
nyarlathotepTheFacelessWhisperer =
  unique
    $ (enemy "06307" ("Nyarlathotep" <:> "The Faceless Whisperer") WhereTheGodsDwell 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepMessengerOfTheOuterGods :: CardDef
nyarlathotepMessengerOfTheOuterGods =
  unique
    $ (enemy "06308" ("Nyarlathotep" <:> "Messenger of the Outer Gods") WhereTheGodsDwell 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Alert]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepGodOfAThousandForms :: CardDef
nyarlathotepGodOfAThousandForms =
  unique
    $ (enemy "06309" ("Nyarlathotep" <:> "God of a Thousand Forms") WhereTheGodsDwell 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepStalkerAmongTheStars :: CardDef
nyarlathotepStalkerAmongTheStars =
  unique
    $ (enemy "06310" ("Nyarlathotep" <:> "Stalker Among the Stars") WhereTheGodsDwell 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Massive]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

highPriestNotToBeDescribed :: CardDef
highPriestNotToBeDescribed =
  unique
    $ (enemy "06311" ("High Priest Not to Be Described" <:> "Agent of the Other Gods") WhereTheGodsDwell 1)
      { cdCardTraits = setFromList [Monster, Cultist, Avatar, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

dholeOfTheWastes :: CardDef
dholeOfTheWastes =
  (enemy "06312" "Dhole of the Wastes" WhereTheGodsDwell 1)
    { cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

liarWithNoFace :: CardDef
liarWithNoFace =
  (enemy "06313" "Liar with No Face" WhereTheGodsDwell 3)
    { cdCardTraits = setFromList [Monster, Cultist, Servitor]
    , cdKeywords = singleton Keyword.Hunter
    }

atlachNacha :: CardDef
atlachNacha =
  unique
    $ (enemy "06346" ("Atlach-Nacha" <:> "The Spider God") WeaverOfTheCosmos 1)
      { cdCardTraits = setFromList [AncientOne, Spider, Elite]
      , cdKeywords = singleton Keyword.Massive
      , cdVictoryPoints = Just 1
      , cdDoubleSided = True
      , cdOtherSide = Just "06346b"
      }

legsOfAtlachNacha_347 :: CardDef
legsOfAtlachNacha_347 =
  (enemy "06347" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_348 :: CardDef
legsOfAtlachNacha_348 =
  (enemy "06348" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_349 :: CardDef
legsOfAtlachNacha_349 =
  (enemy "06349" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_350 :: CardDef
legsOfAtlachNacha_350 =
  (enemy "06350" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

webSpinner :: CardDef
webSpinner =
  (enemy "06351" "Web-Spinner" WeaverOfTheCosmos 3)
    { cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = singleton Keyword.Aloof
    }

shadowAgents :: CardDef
shadowAgents =
  (weakness "07011" "Shadow Agents")
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    }

accursedFollower :: CardDef
accursedFollower =
  (basicWeakness "07038" "Accursed Follower")
    { cdCardTraits = setFromList [Humanoid, Cultist, Cursed]
    , cdKeywords = singleton Keyword.Aloof
    }

theAmalgam :: CardDef
theAmalgam =
  unique
    $ (enemy "07053" "The Amalgam" ThePitOfDespair 1)
      { cdCardTraits = setFromList [Monster, Abomination, DeepOne, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }

angryMob :: CardDef
angryMob =
  (enemy "07062b" "Angry Mob" TheVanishingOfElinaHarper 1)
    { cdCardTraits = setFromList [Humanoid, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    }

robertFriendlyDisgruntledDockworker :: CardDef
robertFriendlyDisgruntledDockworker =
  unique
    $ (enemy "07076" ("Robert Friendly" <:> "Disgruntled Dockerworker") TheVanishingOfElinaHarper 1)
      { cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

zadokAllenDrunkAndDisorderly :: CardDef
zadokAllenDrunkAndDisorderly =
  unique
    $ (enemy "07077" ("Zadok Allen" <:> "Drunk and Disorderly") TheVanishingOfElinaHarper 1)
      { cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

brianBurnhamWantsOut :: CardDef
brianBurnhamWantsOut =
  unique
    $ (enemy "07078" ("Brian Burnham" <:> "Wants Out") TheVanishingOfElinaHarper 1)
      { cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

barnabasMarshTheChangeIsUponHim :: CardDef
barnabasMarshTheChangeIsUponHim =
  unique
    $ (enemy "07079" ("Barnabas Marsh" <:> "The Change Is upon Him") TheVanishingOfElinaHarper 1)
      { cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

joyceLittleBookshopOwner :: CardDef
joyceLittleBookshopOwner =
  unique
    $ (enemy "07080" ("Joyce Little" <:> "Bookshop Owner") TheVanishingOfElinaHarper 1)
      { cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

otheraGilmanProprietessOfTheHotel :: CardDef
otheraGilmanProprietessOfTheHotel =
  unique
    $ (enemy "07081" ("Othera Gilman" <:> "Proprietess of the Hotel") TheVanishingOfElinaHarper 1)
      { cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

priestOfDagon :: CardDef
priestOfDagon =
  (enemy "07084" "Priest of Dagon" AgentsOfDagon 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

initiateOfDagon :: CardDef
initiateOfDagon =
  (enemy "07085" "Initiate of Dagon" AgentsOfDagon 3)
    { cdCardTraits = setFromList [Humanoid, Hybrid, Cultist]
    }

lloigor :: CardDef
lloigor =
  (enemy "07086" "Lloigor" AgentsOfHydra 1)
    { cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

deepOneBull :: CardDef
deepOneBull =
  (enemy "07088" "Deep One Bull" CreaturesOfTheDeep 1)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }

lurkingDeepOne :: CardDef
lurkingDeepOne =
  (enemy "07089" "Lurking Deep One" CreaturesOfTheDeep 3)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }

wingedOneFogOverInnsmouth :: CardDef
wingedOneFogOverInnsmouth =
  (enemy "07094" "Winged One" FogOverInnsmouth 1)
    { cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton Keyword.Hunter
    }

innsmouthTroublemaker :: CardDef
innsmouthTroublemaker =
  (enemy "07105" "Innsmouth Troublemaker" TheLocals 2)
    { cdCardTraits = setFromList [Humanoid, Hybrid, Criminal]
    , cdKeywords = singleton Keyword.Hunter
    }

innsmouthShoggoth :: CardDef
innsmouthShoggoth =
  (enemy "07144" "Innsmouth Shoggoth" InTooDeep 1)
    { cdCardTraits = setFromList [Monster, Shoggoth, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

ravagerFromTheDeep :: CardDef
ravagerFromTheDeep =
  (enemy "07145" "Ravager from the Deep" InTooDeep 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

emergingDeepOne :: CardDef
emergingDeepOne =
  (enemy "07146" "Emerging Deep One" InTooDeep 3)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdRevelation = IsRevelation
    }

theTerrorOfDevilReef_164 :: CardDef
theTerrorOfDevilReef_164 =
  (enemy "07164b" "The Terror of Devil Reef" DevilReef 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

theTerrorOfDevilReef_165 :: CardDef
theTerrorOfDevilReef_165 =
  (enemy "07165b" "The Terror of Devil Reef" DevilReef 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    }

deepOnePredator :: CardDef
deepOnePredator =
  (enemy "07182" "Deep One Predator" DevilReef 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = singleton Keyword.Hunter
    }

huntingDeepOne :: CardDef
huntingDeepOne =
  (enemy "07183" "Hunting Deep One" DevilReef 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = singleton Keyword.Hunter
    }

theTerrorOfDevilReefRelentlessMonstrosity :: CardDef
theTerrorOfDevilReefRelentlessMonstrosity =
  (enemy "07199b" ("The Terror of Devil Reef" <:> "Relentless Monstrosity") HorrorInHighGear 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    }

pursuingMotorcar :: CardDef
pursuingMotorcar =
  (enemy "07213" "Pursuing Motorcar" HorrorInHighGear 2)
    { cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

hitVan :: CardDef
hitVan =
  (enemy "07214" "Hit Van" HorrorInHighGear 2)
    { cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

hybridAssassin :: CardDef
hybridAssassin =
  (enemy "07215" "Hybrid Assassin" HorrorInHighGear 2)
    { cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

oceirosMarsh :: CardDef
oceirosMarsh =
  (enemy "07253" ("Oceiros Marsh" <:> "Keeper of the Lighthouse") ALightInTheFog 1)
    { cdCardTraits = setFromList [Humanoid, DeepOne, Hybrid, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

deepOneNursemaid :: CardDef
deepOneNursemaid =
  (enemy "07254" "Deep One Nursemaid" ALightInTheFog 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }

deepOneHatchling :: CardDef
deepOneHatchling =
  (enemy "07255" "Deep One Hatchling" ALightInTheFog 4)
    { cdCardTraits = setFromList [Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Surge]
    }

dagonDeepInSlumber :: CardDef
dagonDeepInSlumber =
  (enemy "07292" ("Dagon" <:> "Deep in Slumber") TheLairOfDagon 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "07292b"
    }

dagonAwakenedAndEnraged :: CardDef
dagonAwakenedAndEnraged =
  (enemy "07292b" ("Dagon" <:> "Awakened and Enraged") TheLairOfDagon 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    , cdDoubleSided = True
    , cdOtherSide = Just "07292"
    }

apostleOfDagon :: CardDef
apostleOfDagon =
  (enemy "07293" "Apostle of Dagon" TheLairOfDagon 1)
    { cdCardTraits = setFromList [Humanoid, Hybrid, Cultist]
    , cdVictoryPoints = Just 1
    }

cerenerianDeepOne :: CardDef
cerenerianDeepOne =
  (enemy "07294" "Cerenerian Deep One" TheLairOfDagon 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

dagonDeepInSlumberIntoTheMaelstrom :: CardDef
dagonDeepInSlumberIntoTheMaelstrom =
  (enemy "07330" ("Dagon" <:> "Deep in Slumber") IntoTheMaelstrom 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "07330b"
    }

dagonAwakenedAndEnragedIntoTheMaelstrom :: CardDef
dagonAwakenedAndEnragedIntoTheMaelstrom =
  (enemy "07330b" ("Dagon" <:> "Awakened and Enraged") IntoTheMaelstrom 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "07330"
    }

hydraDeepInSlumber :: CardDef
hydraDeepInSlumber =
  (enemy "07331" ("Hydra" <:> "Deep in Slumber") IntoTheMaelstrom 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "07331b"
    }

hydraAwakenedAndEnraged :: CardDef
hydraAwakenedAndEnraged =
  (enemy "07331b" ("Hydra" <:> "Awakened and Enraged") IntoTheMaelstrom 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "07330"
    }

aquaticAbomination :: CardDef
aquaticAbomination =
  (enemy "07332" "Aquatic Abomination" IntoTheMaelstrom 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

dagonsBrood :: CardDef
dagonsBrood =
  (enemy "07333" "Dagon's Brood" IntoTheMaelstrom 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

hydrasBrood :: CardDef
hydrasBrood =
  (enemy "07334" "Hydra's Brood" IntoTheMaelstrom 2)
    { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

mobGoons :: CardDef
mobGoons =
  (weakness "08003" "Mob Goons")
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = singleton Keyword.Hunter
    }

skitteringNonsense :: CardDef
skitteringNonsense =
  (enemy "08515" "Skittering Nonsense" IceAndDeath 3)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

terrorOfTheStarsBringerOfIceAndDeath :: CardDef
terrorOfTheStarsBringerOfIceAndDeath =
  (enemy "08522" ("Terror of the Stars" <:> "Bringer of Ice and Death") TheCrash 1)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

professorWilliamDyerProfessorOfGeology :: CardDef
professorWilliamDyerProfessorOfGeology =
  (enemy "08535" ("Professor William Dyer" <:> "Professor of Geology") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

danforthBrilliantStudent :: CardDef
danforthBrilliantStudent =
  (enemy "08536" ("Danforth" <:> "Brilliant Student") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

eliyahAshevakDogHandler :: CardDef
eliyahAshevakDogHandler =
  (enemy "08537" ("Eliyah Ashevak" <:> "Dog Handler") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

drMalaSinhaDaringPhysician :: CardDef
drMalaSinhaDaringPhysician =
  (enemy "08538" ("Dr. Mala Sinha" <:> "Daring Physician") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

averyClaypoolAntarcticGuide :: CardDef
averyClaypoolAntarcticGuide =
  (enemy "08539" ("Avery Claypool" <:> "Antarctic Guide") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

jamesCookieFredericksDubiousChoice :: CardDef
jamesCookieFredericksDubiousChoice =
  (enemy "08540" ("James \"Cookie\" Fredericks" <:> "Dubious Choice") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdKeywords = singleton Keyword.Retaliate
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

drAmyKenslerProfessorOfBiology :: CardDef
drAmyKenslerProfessorOfBiology =
  (enemy "08541" ("Dr. Amy Kensler" <:> "Professor of Biology") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

roaldEllsworthIntrepidExplorer :: CardDef
roaldEllsworthIntrepidExplorer =
  (enemy "08542" ("Roald Ellsworth" <:> "Intrepid Explorer") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

takadaHirokoAeroplaneMechanic :: CardDef
takadaHirokoAeroplaneMechanic =
  (enemy "08543" ("Takada Hiroko" <:> "Aeroplane Mechanic") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

seepingNightmare :: CardDef
seepingNightmare =
  (enemy "08547" "Seeping Nightmare" SeepingNightmares 4)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    }

memoryOfAHuntGoneAwry :: CardDef
memoryOfAHuntGoneAwry =
  doubleSided "08575b"
    $ (enemy "08575" "Memory of a Hunt Gone Awry" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      }

memoryOfALostPatient :: CardDef
memoryOfALostPatient =
  doubleSided "08576b"
    $ (enemy "08576" "Memory of a Lost Patient" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfAMissingFather :: CardDef
memoryOfAMissingFather =
  doubleSided "08577b"
    $ (enemy "08577" "Memory of a Missing Father" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfARavagedCountry :: CardDef
memoryOfARavagedCountry =
  doubleSided "08578b"
    $ (enemy "08578" "Memory of a Ravaged Country" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfARegretfulVoyage :: CardDef
memoryOfARegretfulVoyage =
  doubleSided "08579b"
    $ (enemy "08579" "Memory of a Regretful Voyage" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfAnUnspeakableEvil :: CardDef
memoryOfAnUnspeakableEvil =
  doubleSided "08580b"
    $ (enemy "08580" "Memory of an Unspeakable Evil" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      }

memoryOfATerribleDiscovery :: CardDef
memoryOfATerribleDiscovery =
  doubleSided "08581b"
    $ (enemy "08581" "Memory of a Terrible Discovery" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

memoryOfAnAlienTranslation :: CardDef
memoryOfAnAlienTranslation =
  doubleSided "08582b"
    $ (enemy "08582" "Memory of an Alien Transformation" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

memoryOfAnUnrequitedLove :: CardDef
memoryOfAnUnrequitedLove =
  doubleSided "08583b"
    $ (enemy "08583" "Memory of an Unrequited Love" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

horrifyingShade :: CardDef
horrifyingShade =
  (enemy "08584" "Horrifying Shade" FatalMirage 3)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

terrorOfTheStarsGuardianOfForbiddenPeaks :: CardDef
terrorOfTheStarsGuardianOfForbiddenPeaks =
  (enemy "08608" ("Terror of the Stars" <:> "Guardian of the Forbidden Peaks") ToTheForbiddenPeaks 1)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

constrictingElderThing :: CardDef
constrictingElderThing =
  (enemy "08609" "Constricting Elder Thing" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

terrorOfTheStarsBaneOfTheElderThings :: CardDef
terrorOfTheStarsBaneOfTheElderThings =
  (enemy "08641" ("Terror of the Stars" <:> "Bane of the Elder Things") CityOfTheElderThings 1)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

benignElderThing :: CardDef
benignElderThing =
  (enemy "08642" "Benign Elder Thing" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Monster, ElderThing]
    }

reawakenedElderThing :: CardDef
reawakenedElderThing =
  (enemy "08643" "Reawakened48;46;178;1656;284848;46;178;1656;2848 Elder Thing" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

protoplasmicMass :: CardDef
protoplasmicMass =
  (enemy "08669" "Protoplasmic Mass" TheGreatSeal 2)
    { cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theNamelessMadness :: CardDef
theNamelessMadness =
  (enemy "08679" "The Nameless Madness" StirringInTheDeep 15)
    { cdCardTraits = setFromList [AncientOne, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Retaliate]
    }

unsealedPhantasm :: CardDef
unsealedPhantasm =
  (enemy "08680" "Unsealed Phantasm" StirringInTheDeep 2)
    { cdCardTraits = setFromList [Monster, Eidolon, Shoggoth]
    , cdVictoryPoints = Just 1
    }

primordialEvil :: CardDef
primordialEvil =
  (enemy "08687" "Primordial Evil" AgentsOfTheUnknown 2)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

manifestationOfMadness :: CardDef
manifestationOfMadness =
  (enemy "08689" "Manifestation of Madness" CreaturesInTheIce 3)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

glacialPhantasm :: CardDef
glacialPhantasm =
  (enemy "08690" "Glacial Phantasm" CreaturesInTheIce 2)
    { cdCardTraits = setFromList [Monster, Eidolon]
    }

elderThingScavenger :: CardDef
elderThingScavenger =
  (enemy "08695" "Elder Thing Scavenger" ElderThings 2)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

guardianElderThing :: CardDef
guardianElderThing =
  (enemy "08696" "Guardian Elder Thing" ElderThings 2)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

lostResearcher :: CardDef
lostResearcher =
  (enemy "08700" "Lost Researcher" LeftBehind 2)
    { cdCardTraits = setFromList [Humanoid, Possessed]
    }

frenziedExplorer :: CardDef
frenziedExplorer =
  (enemy "08701" "Frenzied Explorer" LeftBehind 2)
    { cdCardTraits = setFromList [Humanoid, Possessed]
    }

giantAlbinoPenguin :: CardDef
giantAlbinoPenguin =
  (enemy "08708" "Giant Albino Penguin" Penguins 2)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

forgottenShoggoth :: CardDef
forgottenShoggoth =
  (enemy "08710" "Forgotten Shoggoth" Shoggoths 2)
    { cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

rampagingShoggoth :: CardDef
rampagingShoggoth =
  (enemy "08711" "Rampaging Shoggoth" Shoggoths 1)
    { cdCardTraits = setFromList [Monster, Shoggoth, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

agentFletcher :: CardDef
agentFletcher =
  (weakness "09010" "Agent Fletcher")
    { cdCardTraits = setFromList [Humanoid, Coterie, Detective]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

lurkerInTheDark :: CardDef
lurkerInTheDark =
  (basicWeakness "09124" "Lurker in the Dark")
    { cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdDeckRestrictions = [OnlyClass Guardian]
    }

ectoplasmicHorror :: CardDef
ectoplasmicHorror =
  (basicWeakness "09127" "Ectoplasmic Horror")
    { cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdDeckRestrictions = [OnlyClass Mystic]
    }

zamacona :: CardDef
zamacona =
  (weakness "10011" "Zamacona")
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Elusive]
    }

weepingYurei :: CardDef
weepingYurei =
  (weakness "10014" "Weeping Yurei")
    { cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Elusive, Keyword.Hunter]
    }

biancaDieKatz :: CardDef
biancaDieKatz =
  (weakness "10063" "Bianca \"Die Katz\"")
    { cdCardTraits = setFromList [Humanoid, Criminal, Socialite]
    , cdKeywords = setFromList [Keyword.Bonded 1 "10062", Keyword.Hunter]
    , cdVictoryPoints = Just 0
    }

corpseHungryGhoul :: CardDef
corpseHungryGhoul =
  (enemy "50022" "Corpse-Hungry Ghoul" ReturnToTheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

ghoulFromTheDepths :: CardDef
ghoulFromTheDepths =
  (enemy "50023" "Ghoul from the Depths" ReturnToTheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

narogath :: CardDef
narogath =
  unique
    $ (enemy "50026b" ("Narôgath" <:> "The Charnel Lord") ReturnToTheMidnightMasks 1)
      { cdCardTraits = setFromList [Humanoid, Monster, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }

graveEater :: CardDef
graveEater =
  (enemy "50038" "Grave-Eater" GhoulsOfUmordhoth 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

acolyteOfUmordhoth :: CardDef
acolyteOfUmordhoth =
  (enemy "50039" "Acolyte of Umôrdhoth" GhoulsOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

discipleOfTheDevourer :: CardDef
discipleOfTheDevourer =
  (enemy "50041" "Disciple of the Devourer" TheDevourersCult 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

corpseTaker :: CardDef
corpseTaker =
  (enemy "50042" "Corpse-Taker" TheDevourersCult 1)
    { cdCardTraits = setFromList [Monster, Servitor, Cultist]
    }

jeremiahPierce :: CardDef
jeremiahPierce =
  unique
    $ ( enemy
          "50044"
          ("Jeremiah Pierce" <:> "Your Next-Door Neighbor")
          ReturnCultOfUmordhoth
          1
      )
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

billyCooper :: CardDef
billyCooper =
  unique
    $ (enemy "50045" ("Billy Cooper" <:> "The Crooked Cop") ReturnCultOfUmordhoth 1)
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

almaHill :: CardDef
almaHill =
  unique
    $ ( enemy
          "50046"
          ("Alma Hill" <:> "The Inquisitive Historian")
          ReturnCultOfUmordhoth
          1
      )
      { cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

enthralledSecurityGuard :: CardDef
enthralledSecurityGuard =
  (enemy "51014" "Entralled Security Guard" ReturnToExtracurricularActivities 2)
    { cdCardTraits = setFromList [Humanoid, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

theConductorBeastFromBeyondTheGate :: CardDef
theConductorBeastFromBeyondTheGate =
  (enemy "51026b" ("The Conductor" <:> "Beast from beyond the Gate") ReturnToTheEssexCountyExpress 1)
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    }

hiredGun :: CardDef
hiredGun =
  (enemy "51040" "Hired Gun" ReturnToBloodOnTheAltar 2)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }

broodOfYogSothothChargingBeast :: CardDef
broodOfYogSothothChargingBeast =
  (enemy "51042" ("Brood of Yog-Sothoth" <:> "Charging Beast") ReturnToUndimensionedAndUnseen 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothSwellingDevourer :: CardDef
broodOfYogSothothSwellingDevourer =
  (enemy "51043" ("Brood of Yog-Sothoth" <:> "Swelling Devourer") ReturnToUndimensionedAndUnseen 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothThrashingSpawn :: CardDef
broodOfYogSothothThrashingSpawn =
  (enemy "51044" ("Brood of Yog-Sothoth" <:> "Thrashing Spawn") ReturnToUndimensionedAndUnseen 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothAmorphousTerror :: CardDef
broodOfYogSothothAmorphousTerror =
  (enemy "51045" ("Brood of Yog-Sothoth" <:> "Amorphous Terror") ReturnToUndimensionedAndUnseen 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

sethBishopThrallOfYogSothoth :: CardDef
sethBishopThrallOfYogSothoth =
  (enemy "51056" ("Seth Bishop" <:> "Thrall of Yog-Sothoth") ReturnToLostInTimeAndSpace 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Abomination, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

vassalOfTheLurker :: CardDef
vassalOfTheLurker =
  (enemy "51071" "Vassal of the Lurker" YogSothothsEmissaries 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

laComtesseSubverterOfPlans :: CardDef
laComtesseSubverterOfPlans =
  unique
    $ (enemy "52020" ("La Comtesse" <:> "Subverter of Plans") ReturnToCurtainCall 1)
      { cdCardTraits = setFromList [Humanoid, Servitor]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

dianneDevineKnowsWhatYoureUpTo :: CardDef
dianneDevineKnowsWhatYoureUpTo =
  unique
    $ (enemy "52023b" ("Dianne Devine" <:> "Knows What You're Up To") ReturnToTheLastKing 1)
      { cdCardTraits = setFromList [Monster, Cultist, Lunatic, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

crazedGuest :: CardDef
crazedGuest =
  (enemy "52024b" "Crazed Guest" ReturnToTheLastKing 2)
    { cdCardTraits = setFromList [Humanoid, Monster, Lunatic]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    }

keeperOfTheOath :: CardDef
keeperOfTheOath =
  (enemy "52033" "Keeper of the Oath" ReturnToEchoesOfThePast 2)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 0
    }

hostOfInsanity :: CardDef
hostOfInsanity =
  (enemy "52037" "Host of Insanity" ReturnToTheUnspeakableOath 1)
    { cdCardTraits = setFromList [Avatar, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

malformedSkeleton :: CardDef
malformedSkeleton =
  (enemy "52053" "Malformed Skeleton" ReturnToThePallidMask 1)
    { cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

highPriestOfHastur :: CardDef
highPriestOfHastur =
  (enemy "52064" "High Priest of Hastur" ReturnToDimCarcosa 1)
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

maggotSwarm :: CardDef
maggotSwarm =
  (enemy "52068" "Maggot Swarm" DecayingReality 2)
    { cdCardTraits = setFromList [Creature]
    }

preyingByakhee :: CardDef
preyingByakhee =
  (enemy "52069" "Preying Byakhee" HastursEnvoys 2)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

harbingerOfValusiaTheSleeperReturns :: CardDef
harbingerOfValusiaTheSleeperReturns =
  unique
    $ ( enemy
          "53018"
          ("Harbinger of Valusia" <:> "The Sleeper Returns")
          ReturnToTheDoomOfEztli
          1
      )
      { cdCardTraits = setFromList [Humanoid, Serpent, Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
      , cdVengeancePoints = Just 5
      }

theWingedSerpentTheFuryOfYig :: CardDef
theWingedSerpentTheFuryOfYig =
  (enemy "53046b" ("The Winged Serpent" <:> "The Fury of Yig") PillarsOfJudgement 1)
    { cdCardTraits = setFromList [Monster, Serpent, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    }

featheredSerpent :: CardDef
featheredSerpent =
  (enemy "53047" "Feathered Serpent" ReturnToPillarsOfJudgement 2)
    { cdCardTraits = setFromList [Creature, Serpent]
    , cdVengeancePoints = Just 1
    }


brotherhoodAcolyte :: CardDef
brotherhoodAcolyte =
  (enemy "53071" "Brotherhood Acolyte" CultOfPnakotus 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

stolenMind :: CardDef
stolenMind =
  (enemy "53072" "Stolen Mind" CultOfPnakotus 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

tindalosAlpha :: CardDef
tindalosAlpha =
  (enemy "53077" "Tindalos Alpha" TemporalHunters 2)
    { cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    }

vengefulSerpent :: CardDef
vengefulSerpent =
  (enemy "53078" "Vengeful Serpent" VenomousHate 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVengeancePoints = Just 0
    }

serpentGuardian :: CardDef
serpentGuardian =
  (enemy "53079" "Serpent Guardian" VenomousHate 1)
    { cdCardTraits = setFromList [Construct, Serpent]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

tommyMalloy :: CardDef
tommyMalloy =
  unique
    $ (weakness "60103" "Tommy Malloy")
      { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

valeriyaAntonovaDontMessWithHer :: CardDef
valeriyaAntonovaDontMessWithHer =
  unique
    $ (enemy "71016b" ("Valeriya Antonova" <:> "Don't Mess With Her") TheMidwinterGala 1)
        { cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
        , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
        , cdDoubleSided = True
        , cdOtherSide = Just "71016"
        , cdVictoryPoints = Just 0
        }

rookieCop :: CardDef
rookieCop =
  (enemy "71020" "Rookie Cop" TheMidwinterGala 1)
    { cdCardTraits = setFromList [Humanoid, Police, Rival]
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Aloof, Keyword.Hunter]
    }

caldwellPhilipsCompelledByDreams :: CardDef
caldwellPhilipsCompelledByDreams =
  unique
    $ (enemy "71022b" ("Caldwell Philips" <:> "Compelled by Dreams") TheMidwinterGala 1)
        { cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
        , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
        , cdDoubleSided = True
        , cdOtherSide = Just "71022"
        , cdVictoryPoints = Just 0
        }

johnnyValoneHereToCollect :: CardDef
johnnyValoneHereToCollect =
  unique
    $ (enemy "71028b" ("Johnny Valone" <:> "Here to Collect") TheMidwinterGala 1)
        { cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
        , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
        , cdDoubleSided = True
        , cdOtherSide = Just "71028"
        , cdVictoryPoints = Just 0
        }

carlSanfordIntimidatingPresence :: CardDef
carlSanfordIntimidatingPresence =
  unique
    $ (enemy "71034b" ("Carl Sanford" <:> "Intimidating Presence") TheMidwinterGala 1)
        { cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
        , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
        , cdDoubleSided = True
        , cdOtherSide = Just "71034"
        , cdVictoryPoints = Just 0
        }

williamBainDefiantToTheLast :: CardDef
williamBainDefiantToTheLast =
  unique
    $ (enemy "71040b" ("William Bain" <:> "Defiant to the Last") TheMidwinterGala 1)
        { cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
        , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
        , cdDoubleSided = True
        , cdOtherSide = Just "71040"
        , cdVictoryPoints = Just 0
        }

theBloodlessMan :: CardDef
theBloodlessMan =
  unique
    $ (enemy "71045" "The Bloodless Man" TheMidwinterGala 1)
        { cdCardTraits = setFromList [Humanoid, LanternClub, Elite]
        , cdKeywords =
            setFromList
              [ Keyword.Aloof
              , Keyword.Patrol (LocationWithAsset $ AssetWithTrait Guest)
              ]
        , cdDoubleSided = True
        , cdOtherSide = Just "71045b"
        }

theBloodlessManUnleashed :: CardDef
theBloodlessManUnleashed =
  unique
    $ (enemy "71045b" "The Bloodless Man" TheMidwinterGala 1)
        { cdCardTraits = setFromList [Monster, Abomination, Elite]
        , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
        , cdVictoryPoints = Just 1
        }

enragedGug :: CardDef
enragedGug =
  (enemy "71047" "Enraged Gug" TheMidwinterGala 1)
    { cdCardTraits = setFromList [Monster, Dreamlands, Gug]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

abhorrentMoonBeast :: CardDef
abhorrentMoonBeast =
  (enemy "71048" "Abhorrent Moon-Beast" TheMidwinterGala 1)
    { cdCardTraits = setFromList [Monster, Dreamlands, Servitor]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

savageShantak :: CardDef
savageShantak =
  (enemy "71049" "Savage Shantak" TheMidwinterGala 1)
    { cdCardTraits = setFromList [Monster, Dreamlands, Shantak]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

declanPearce :: CardDef
declanPearce =
  unique
    $ (enemy "71051" "Declan Pearce" TheMidwinterGala 1)
        { cdCardTraits = setFromList [Humanoid, LanternClub, Elite]
        , cdKeywords = setFromList [Keyword.Hunter]
        , cdVictoryPoints = Just 0
        }

lanternClubMember :: CardDef
lanternClubMember =
  (enemy "71053" "Lantern Club Member" TheMidwinterGala 4)
    { cdCardTraits = setFromList [Humanoid, LanternClub]
    }

bogGator :: CardDef
bogGator =
  (enemy "81022" "Bog Gator" TheBayou 2)
    { cdCardTraits = setFromList [Creature]
    }

swampLeech :: CardDef
swampLeech =
  (enemy "81023" "Swamp Leech" TheBayou 3)
    { cdCardTraits = setFromList [Creature]
    }

theRougarou :: CardDef
theRougarou =
  unique
    $ (enemy "81028" ("The Rougarou" <:> "Cursed Soul") CurseOfTheRougarou 1)
      { cdCardTraits = setFromList [Monster, Creature, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }

slimeCoveredDhole :: CardDef
slimeCoveredDhole =
  (enemy "81031" "Slime-Covered Dhole" CurseOfTheRougarou 2)
    { cdCardTraits = setFromList [Monster, Dhole]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

marshGug :: CardDef
marshGug =
  (enemy "81032" "Marsh Gug" CurseOfTheRougarou 2)
    { cdCardTraits = setFromList [Monster, Gug]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

darkYoungHost :: CardDef
darkYoungHost =
  (enemy "81033" "Dark Young Host" CurseOfTheRougarou 1)
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

balefulReveler :: CardDef
balefulReveler =
  unique
    $ (enemy "82002b" ("Baleful Reveler" <:> "Spreading Chaos") CarnevaleOfHorrors 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

donLagorio :: CardDef
donLagorio =
  unique
    $ (enemy "82017" ("Don Lagorio" <:> "Secret Servant") CarnevaleOfHorrors 1)
      { cdCardTraits = setFromList [Humanoid, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      , cdDoubleSided = True
      , cdOtherSide = Just "82017b"
      }

elisabettaMagro :: CardDef
elisabettaMagro =
  unique
    $ ( enemy
          "82018"
          ("Elisabetta Magro" <:> "High Servant of the Order")
          CarnevaleOfHorrors
          1
      )
      { cdCardTraits = setFromList [Humanoid, Lodge, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 1
      , cdDoubleSided = True
      , cdOtherSide = Just "82018b"
      }

salvatoreNeri :: CardDef
salvatoreNeri =
  unique
    $ ( enemy
          "82019"
          ("Salvatore Neri" <:> "Master of Illusions")
          CarnevaleOfHorrors
          1
      )
      { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      , cdDoubleSided = True
      , cdOtherSide = Just "82019b"
      }

savioCorvi :: CardDef
savioCorvi =
  unique
    $ (enemy "82020" ("Savio Corvi" <:> "Dark Lurker") CarnevaleOfHorrors 1)
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      , cdDoubleSided = True
      , cdOtherSide = Just "82020b"
      }

cnidathqua :: CardDef
cnidathqua =
  unique
    $ (enemy "82027" ("Cnidathqua" <:> "The Many-armed Beast") CarnevaleOfHorrors 1)
      { cdCardTraits = setFromList [Monster, AncientOne, Elite]
      }

poleman :: CardDef
poleman =
  (enemy "82028" "Poleman" CarnevaleOfHorrors 2)
    { cdCardTraits = setFromList [Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

carnevaleSentinel :: CardDef
carnevaleSentinel =
  (enemy "82029" "Carnevale Sentinel" CarnevaleOfHorrors 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

writhingAppendage :: CardDef
writhingAppendage =
  (enemy "82030" "Writhing Appendage" CarnevaleOfHorrors 3)
    { cdCardTraits = setFromList [Monster, Tentacle]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

arkhamOfficer :: CardDef
arkhamOfficer =
  (enemy "84009" "Arkham Officer" MurderAtTheExcelsiorHotel 3)
    { cdCardTraits = setFromList [Humanoid, Police, Innocent]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol LocationWithAnyClues]
    , cdVictoryPoints = Just 0
    }

mrTrombly :: CardDef
mrTrombly =
  (enemy "84020" ("Mr. Trombly" <:> "Maddened Concierge") MurderAtTheExcelsiorHotel 1)
    { cdCardTraits = setFromList [Humanoid, Staff]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

conspicuousStaff :: CardDef
conspicuousStaff =
  (enemy "84021" "Conspicuous Staff" MurderAtTheExcelsiorHotel 3)
    { cdCardTraits = setFromList [Humanoid, Staff]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

hotelGuest :: CardDef
hotelGuest =
  (enemy "84022" "Hotel Guest" MurderAtTheExcelsiorHotel 4)
    { cdCardTraits = setFromList [Humanoid, Guest, Innocent]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTrait CrimeScene)]
    , cdVictoryPoints = Just 0
    }

otherworldlyMeddler :: CardDef
otherworldlyMeddler =
  (enemy "84029" ("Otherworldly Meddler" <:> "Presence from Beyond the Stars") AlienInterference 1)
    { cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

hotelManager :: CardDef
hotelManager =
  (enemy "84032" ("Hotel Manager" <:> "Let the Feast Begin") ExcelsiorManagement 1)
    { cdCardTraits = setFromList [Monster, Staff, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

hotelSecurity :: CardDef
hotelSecurity =
  (enemy "84033" "Hotel Security" ExcelsiorManagement 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Staff]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

dimensionalShambler :: CardDef
dimensionalShambler =
  (enemy "84035" "Dimensional Shambler" DarkRituals 1)
    { cdCardTraits = setFromList [Monster, Extradimensional, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 2
    }

cultistOfTheEnclave :: CardDef
cultistOfTheEnclave =
  (enemy "84036" "Cultist of the Enclave" DarkRituals 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

vengefulSpecter :: CardDef
vengefulSpecter =
  (enemy "84041" ("Vengeful Specter" <:> "The First Victim") SinsOfThePast 1)
    { cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdKeywords = setFromList [Keyword.Patrol "Room 245", Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

vengefulShade :: CardDef
vengefulShade =
  (weakness "90053" "Vengeful Shade")
    { cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = singleton Keyword.Hunter
    }

sacrificialBeast :: CardDef
sacrificialBeast =
  (weakness "98003" "Sacrificial Beast")
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdKeywords = singleton Keyword.Replacement
    }

vengefulHound :: CardDef
vengefulHound =
  (weakness "98009" "Vengeful Hound")
    { cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = singleton Keyword.Replacement
    }

serpentsOfYigAdvanced :: CardDef
serpentsOfYigAdvanced =
  (weakness "90083" "Serpents of Yig")
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Advanced]
    , cdRevelation = IsRevelation
    }

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
