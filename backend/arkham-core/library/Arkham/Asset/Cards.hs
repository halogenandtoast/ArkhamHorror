module Arkham.Asset.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses hiding (Key)
import Arkham.Asset.Uses qualified as Uses
import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ChaosToken qualified as Token
import Arkham.ClassSymbol
import Arkham.CommitRestriction
import Arkham.Cost
import Arkham.Criteria qualified as Criteria
import Arkham.Customization
import Arkham.EncounterSet hiding (Dreamlands, Dunwich)
import Arkham.GameValue
import Arkham.Keyword (Keyword, Sealing (..))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Name
import Arkham.Trait hiding (Evidence, Supply)

storyAsset :: CardCode -> Name -> Int -> EncounterSet -> CardDef
storyAsset cardCode name cost encounterSet =
  baseAsset (Just (encounterSet, 1)) cardCode name cost (singleton Neutral)

storyAssetWithMany :: CardCode -> Name -> Int -> EncounterSet -> Int -> CardDef
storyAssetWithMany cardCode name cost encounterSet encounterSetCount =
  baseAsset (Just (encounterSet, encounterSetCount)) cardCode name cost (singleton Neutral)

asset :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
asset cCode name cost classSymbol = baseAsset Nothing cCode name cost (singleton classSymbol)

multiClassAsset :: CardCode -> Name -> Int -> [ClassSymbol] -> CardDef
multiClassAsset cCode name cost classSymbols = baseAsset Nothing cCode name cost (setFromList classSymbols)

permanent :: CardDef -> CardDef
permanent cd = cd {cdPermanent = True, cdCost = Nothing}

fast :: CardDef -> CardDef
fast cd = cd {cdFastWindow = Just (DuringTurn You)}

weakness :: CardCode -> Name -> CardDef
weakness cardCode name =
  (baseAsset Nothing cardCode name 0 (singleton Neutral))
    { cdCardSubType = Just Weakness
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    }

basicWeakness :: CardCode -> Name -> CardDef
basicWeakness cardCode name =
  (baseAsset Nothing cardCode name 0 (singleton Neutral))
    { cdCardSubType = Just BasicWeakness
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    }

storyWeakness :: CardCode -> Name -> EncounterSet -> CardDef
storyWeakness cardCode name encounterSet =
  (baseAsset (Just (encounterSet, 1)) cardCode name 0 (singleton Neutral))
    { cdCardSubType = Just Weakness
    , cdRevelation = IsRevelation
    , cdCost = Nothing
    }

uses :: UseType -> Int -> Uses GameValue
uses uType = Uses uType . Static

seal :: IsSealing s => s -> Keyword
seal (toSealing -> s) = Keyword.Seal s

class IsSealing s where
  toSealing :: s -> Sealing

instance IsSealing Sealing where
  toSealing = id
  {-# INLINE toSealing #-}

instance IsSealing ChaosTokenMatcher where
  toSealing s = Sealing s
  {-# INLINE toSealing #-}

instance IsSealing Token.ChaosTokenFace where
  toSealing = Sealing . ChaosTokenFaceIs
  {-# INLINE toSealing #-}

baseAsset
  :: Maybe (EncounterSet, Int)
  -> CardCode
  -> Name
  -> Int
  -> Set ClassSymbol
  -> CardDef
baseAsset mEncounterSet cardCode name cost classSymbols =
  (emptyCardDef cardCode name AssetType)
    { cdCost = Just (StaticCost cost)
    , cdClassSymbols = classSymbols
    , cdEncounterSet = fst <$> mEncounterSet
    , cdEncounterSetQuantity = snd <$> mEncounterSet
    }

allPlayerAssetCards :: Map CardCode CardDef
allPlayerAssetCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [ abbessAllegriaDiBiase
      , abigailForeman4
      , abyssalTome2
      , aceOfRods1
      , aceOfSwords1
      , adaptable1
      , agencyBackup5
      , alchemicalTransmutation
      , alchemicalTransmutation2
      , alejandroVela
      , aliceLuxley
      , alyssaGraham
      , analyticalMind
      , ancestralKnowledge3
      , ancientStone1
      , ancientStoneKnowledgeOfTheElders4
      , ancientStoneMindsInHarmony4
      , ancientStoneTransientThoughts4
      , annaKaslow4
      , anotherDayAnotherDollar3
      , aquinnah1
      , aquinnah3
      , arbiterOfFates
      , arcaneEnlightenment
      , arcaneInitiate
      , arcaneInitiate3
      , arcaneInsight4
      , arcaneResearch
      , arcaneStudies
      , arcaneStudies2
      , arcaneStudies4
      , archaicGlyphs
      , archaicGlyphsGuidingStones3
      , archaicGlyphsMarkingsOfIsis3
      , archaicGlyphsProphecyForetold3
      , armorOfArdennes5
      , artStudent
      , augur
      , augustLindquist
      , azureFlame
      , azureFlame3
      , azureFlame5
      , backpack
      , backpack2
      , bandolier
      , bandolier2
      , bangleOfJinxes1
      , baronSamedi
      , baseballBat
      , bauta
      , beatCop
      , beatCop2
      , becky
      , berettaM19184
      , blackjack
      , blackjack2
      , blessedBlade
      , bloodPact3
      , bloodstainedDagger
      , bookOfPsalms
      , bookOfShadows1
      , bookOfShadows3
      , borrowedTime3
      , bountyContracts
      , boxingGloves
      , boxingGloves3
      , brandOfCthugha1
      , brandOfCthugha4
      , brotherXavier1
      , bulletproofVest3
      , burglary
      , burglary2
      , catBurglar1
      , celaenoFragments
      , chainsaw4
      , charisma3
      , charlesRossEsq
      , charonsObol1
      , cherishedKeepsake
      , cherishedKeepsake1
      , chicagoTypewriter4
      , chuckFergus2
      , chuckFergus5
      , clarityOfMind
      , clarityOfMind3
      , clairvoyance
      , clairvoyance3
      , clairvoyance5
      , claspOfBlackOnyx
      , coltVestPocket
      , coltVestPocket2
      , cornered2
      , combatTraining1
      , cryptographicCipher
      , crystalPendulum
      , crystallineElderSign3
      , crystallizerOfDreams
      , daisysToteBag
      , daisysToteBagAdvanced
      , darioElAmin
      , darkHorse
      , davidRenfield
      , dayanaEsperence3
      , deathXiii1
      , decoratedSkull
      , decoratedSkull3
      , dejaVu5
      , delilahORourke3
      , dendromorphosis
      , detectivesColt1911s
      , deVermisMysteriis2
      , digDeep
      , digDeep2
      , discOfItzamna
      , discOfItzamna2
      , doubleDouble4
      , drElliHorowitz
      , drFrancisMorgan
      , drHenryArmitage
      , drMilanChristopher
      , drShivaniMaheswaran
      , drWilliamTMaleson
      , dragonPole
      , drawingThin
      , dreamEnhancingSerum
      , dreamDiary
      , dreamDiaryDreamsOfAChild3
      , dreamDiaryDreamsOfAMadman3
      , dreamDiaryDreamsOfAnExplorer3
      , duke
      , earlSawyer
      , eighteenDerringer
      , eighteenDerringer2
      , elderSignAmulet3
      , empiricalHypothesis
      , empowerSelfAcuity2
      , empowerSelfAlacrity2
      , empowerSelfStamina2
      , emptyVessel4
      , enchantedBlade
      , enchantedBladeGuardian3
      , enchantedBladeMystic3
      , encyclopedia
      , encyclopedia2
      , esotericAtlas1
      , esotericAtlas2
      , esotericFormula
      , expeditionJournal
      , familiarSpirit
      , familyInheritance
      , farsight4
      , feedTheMind
      , feedTheMind3
      , fence1
      , fieldwork
      , fineClothes
      , fingerprintKit
      , fingerprintKit4
      , finnsTrustyThirtyEight
      , fireAxe
      , fireExtinguisher1
      , firstAid
      , firstAid3
      , fiveOfPentacles1
      , flamethrower5
      , flashlight
      , fleshWard
      , forbiddenKnowledge
      , forbiddenTome
      , forbiddenTomeDarkKnowledge3
      , forbiddenTomeSecretsRevealed3
      , forcedLearning
      , fortyFiveAutomatic
      , fortyFiveAutomatic2
      , fortyFiveThompson
      , fortyFiveThompsonGuardian3
      , fortyFiveThompsonRogue3
      , fortyOneDerringer
      , fortyOneDerringer2
      , fourOfCups1
      , garroteWire2
      , gateBox
      , gavriellaMizrah
      , geneBeauregard3
      , gildedVolto
      , grannyOrne
      , grannyOrne3
      , gravediggersShovel
      , gravediggersShovel2
      , gregoryGry
      , greteWagner
      , greteWagner3
      , grimmsFairyTales
      , grislyTotem
      , grislyTotemSeeker3
      , grislyTotemSurvivor3
      , grotesqueStatue2
      , grotesqueStatue4
      , grounded1
      , guardDog
      , guardDog2
      , guardianAngel
      , guidingSpirit1
      , hallowedMirror
      , hallowedMirror3
      , handcuffs
      , handcuffs2
      , harlanEarnstone
      , hardKnocks
      , hardKnocks2
      , hardKnocks4
      , haste2
      , hawkEyeFoldingCamera
      , healingWords
      , heirloomOfHyperborea
      , hemisphericMap3
      , henryDeveau
      , henryWan
      , highRoller2
      , higherEducation
      , higherEducation3
      , hiredMuscle1
      , holyRosary
      , holyRosary2
      , hope
      , hyperawareness
      , hyperawareness2
      , hypnoticTherapy
      , ichtacaTheForgottenGuardian
      , inTheKnow1
      , inTheThickOfIt
      , ineffableTruth
      , ineffableTruth3
      , ineffableTruth5
      , innocentReveler
      , investments
      , jakeWilliams
      , jennysTwin45s
      , jeromeDavids
      , jessicaHyde1
      , jewelOfAureolus3
      , jimsTrumpet
      , joeyTheRatVigil
      , joeyTheRatVigil3
      , keenEye
      , keenEye3
      , kerosene1
      , keyOfYs
      , kleptomania
      , knife
      , knightOfSwords3
      , knuckleduster
      , kukri
      , laboratoryAssistant
      , ladyEsprit
      , lantern
      , lantern2
      , leatherCoat
      , leatherCoat1
      , leatherJacket
      , leoDeLuca
      , leoDeLuca1
      , libraryDocent1
      , lightningGun5
      , liquidCourage
      , liquidCourage1
      , litaChantler
      , livreDeibon
      , lockpicks
      , lockpicks1
      , lolaSantiago3
      , loneWolf
      , lonnieRitter
      , luckyCigaretteCase
      , luckyCigaretteCase3
      , luckyDice2
      , lupara3
      , m1918Bar4
      , machete
      , madameLabranche
      , magnifyingGlass
      , magnifyingGlass1
      , mariaDeSilva
      , maskedCarnevaleGoer_17
      , maskedCarnevaleGoer_18
      , maskedCarnevaleGoer_19
      , maskedCarnevaleGoer_20
      , maskedCarnevaleGoer_21
      , mauserC96
      , mauserC962
      , meatCleaver
      , medicalStudent
      , medicalTexts
      , medicoDellaPeste
      , michaelLeigh5
      , mindsEye2
      , miskatonicArchaeologyFunding4
      , missDoyle1
      , mistsOfRlyeh
      , mistsOfRlyeh2
      , mistsOfRlyeh4
      , mitchBrown
      , mk1Grenades4
      , mollyMaxwell
      , monstrousTransformation
      , moonPendant2
      , moonstone
      , moxie1
      , mrRook
      , mysteriousRaven
      , newspaper
      , newspaper2
      , nightmareBauble3
      , nineOfRods3
      , observed4
      , occultLexicon
      , occultLexicon3
      , occultScraps
      , oldBookOfLore
      , oldBookOfLore3
      , oldHuntingRifle3
      , oldKeyring
      , oliveMcBride
      , onYourOwn3
      , onYourOwn3_Exceptional
      , ornateBow3
      , otherworldCodex2
      , otherworldlyCompass2
      , painkillers
      , pantalone
      , pathfinder1
      , patricesViolin
      , pendantOfTheQueen
      , pennyWhite
      , peterSylvestre
      , peterSylvestre2
      , physicalTraining
      , physicalTraining2
      , physicalTraining4
      , pickpocketing
      , pickpocketing2
      , plucky1
      , pnakoticManuscripts5
      , policeBadge2
      , powderOfIbnGhazi
      , preciousMementoFromAFormerLife4
      , preciousMementoFromAFutureLife4
      , professorWarrenRice
      , protectiveIncantation1
      , puzzleBox
      , quickLearner4
      , quickStudy2
      , rabbitsFoot
      , rabbitsFoot3
      , randallCho
      , randolphCarterChainedToTheWakingWorld
      , randolphCarterExpertDreamer
      , recallTheFuture2
      , relentless
      , relicHunter3
      , relicOfAgesADeviceOfSomeSort
      , relicOfAgesForestallingTheFuture
      , relicOfAgesRepossessThePast
      , relicOfAgesUnleashTheTimestream
      , researchLibrarian
      , riteOfSanctification
      , riteOfSeeking
      , riteOfSeeking2
      , riteOfSeeking4
      , ritualCandles
      , robesOfEndlessNight
      , robesOfEndlessNight2
      , rolands38Special
      , safeguard
      , safeguard2
      , sawedOffShotgun5
      , scavenging
      , scavenging2
      , schoffnersCatalogue
      , scientificTheory1
      , scrapper
      , scrapper3
      , scrollOfProphecies
      , scrollOfSecrets
      , scrollOfSecretsMystic3
      , scrollOfSecretsSeeker3
      , scrying
      , scrying3
      , scryingMirror
      , seaChangeHarpoon
      , sealOfTheSeventhSign5
      , segmentOfOnyx1
      , sergeantMonroe
      , shardsOfTheVoid3
      , sharpshooter3
      , shiningTrapezohedron4
      , shotgun4
      , showmanship
      , shrewdAnalysis
      , shrivelling
      , shrivelling3
      , shrivelling5
      , signMagick
      , signMagick3
      , silassNet
      , sixthSense
      , sixthSense4
      , smokingPipe
      , solemnVow
      , somethingWorthFightingFor
      , sophieInLovingMemory
      , sophieItWasAllMyFault
      , spectralWeb
      , spiritAthame1
      , stealth
      , stealth3
      , stickToThePlan3
      , streetwise
      , streetwise3
      , studious3
      , songOfTheDead2
      , spiritSpeaker
      , spiritualResolve5
      , springfieldM19034
      , stHubertsKey
      , strangeSolution
      , strangeSolutionAcidicIchor4
      , strangeSolutionEmpoweringElixir4
      , strangeSolutionFreezingVariant4
      , strangeSolutionRestorativeConcoction4
      , strayCat
      , suggestion1
      , suggestion4
      , summonedHound1
      , survivalKnife
      , survivalKnife2
      , switchblade
      , switchblade2
      , swordCane
      , tennesseeSourMash
      , tennesseeSourMashRogue3
      , tennesseeSourMashSurvivor3
      , tetsuoMori
      , theBlackBook
      , theBlackCat5
      , theBlackFan3
      , theChthonianStone
      , theChthonianStone3
      , theCodexOfAges
      , theCouncilsCoffer2
      , theCustodian
      , theDevilXv
      , theFool03
      , theGoldPocketWatch4
      , theHierophantV3
      , theHungeringBlade1
      , theKingInYellow
      , theMoonXiii1
      , theNecronomicon
      , theNecronomiconAdvanced
      , theNecronomiconOlausWormiusTranslation
      , theNecronomiconPetrusDeDaciaTranslation5
      , thePallidMask
      , theRedGlovedMan5
      , theSilverKey
      , theSkeletonKey2
      , theStarXvii3
      , theTatteredCloak
      , theTowerXVI
      , theWorldXxi3
      , thermos
      , thirtyFiveWinchester
      , thirtyTwoColt
      , thirtyTwoColt2
      , timewornBrand5
      , tonys38LongColt
      , toothOfEztli
      , trackShoes
      , treasureHunter1
      , trenchCoat
      , trenchKnife
      , trueGrit
      , tryAndTryAgain1
      , tryAndTryAgain3
      , twilaKatherinePrice3
      , twilightBlade
      , untilTheEndOfTime
      , valentinoRivas
      , vaultOfKnowledge
      , venturer
      , versatile2
      , virgilGray
      , wellConnected
      , wellConnected3
      , wellPrepared2
      , wendysAmulet
      , whittonGreene
      , whittonGreene2
      , wishEater
      , wither
      , wither4
      , yaotl1
      , zeal
      , zebulonWhateley
      , zoeysCross
      ]

allEncounterAssetCards :: Map CardCode CardDef
allEncounterAssetCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [ adamLynch
      , alchemicalConcoction
      , bearTrap
      , fishingNet
      , haroldWalsted
      , helplessPassenger
      , jazzMulligan
      , keyToTheChamber
      , peterClover
      , constanceDumaine
      , jordanPerry
      , ishimaruHaruko
      , sebastienMoreau
      , ashleighClarke
      , mrPeabody
      , danielChesterfield
      , alienDevice
      , managersKey
      , tomeOfRituals
      , sinisterSolution
      , timeWornLocket
      , virgilGrayTrulyInspired
      , theCaptain
      , richardUptonPickman
      ]

allSpecialPlayerAssetCards :: Map CardCode CardDef
allSpecialPlayerAssetCards =
  mapFromList $ map (toCardCode &&& id) [courage, straitjacket, intrepid]

rolands38Special :: CardDef
rolands38Special =
  (asset "01006" "Roland's .38 Special" 3 Neutral)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUnique = True
    , cdUses = uses Ammo 4
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01506"]
    , cdDeckRestrictions = [Signature "01001"]
    }

daisysToteBag :: CardDef
daisysToteBag =
  (asset "01008" "Daisy's Tote Bag" 2 Neutral)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Item]
    , cdUnique = True
    , cdAlternateCardCodes = ["01508"]
    , cdDeckRestrictions = [Signature "01002"]
    }

theNecronomicon :: CardDef
theNecronomicon =
  (weakness "01009" ("The Necronomicon" <:> "John Dee Translation"))
    { cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01509"]
    }

heirloomOfHyperborea :: CardDef
heirloomOfHyperborea =
  (asset "01012" ("Heirloom of Hyperborea" <:> "Artifact from Another Life") 3 Neutral)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01512"]
    , cdDeckRestrictions = [Signature "01004"]
    }

wendysAmulet :: CardDef
wendysAmulet =
  (asset "01014" "Wendy's Amulet" 2 Neutral)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01514"]
    , cdDeckRestrictions = [Signature "01005"]
    }

fortyFiveAutomatic :: CardDef
fortyFiveAutomatic =
  (asset "01016" ".45 Automatic" 4 Guardian)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 4
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01516"]
    }

physicalTraining :: CardDef
physicalTraining =
  (asset "01017" "Physical Training" 2 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01517", "60108"]
    }

beatCop :: CardDef
beatCop =
  (asset "01018" "Beat Cop" 4 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Police]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01518"]
    }

firstAid :: CardDef
firstAid =
  (asset "01019" "First Aid" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Talent, Science]
    , cdUses = uses Supply 3
    , cdAlternateCardCodes = ["01519"]
    }

machete :: CardDef
machete =
  (asset "01020" "Machete" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01520"]
    }

guardDog :: CardDef
guardDog =
  (asset "01021" "Guard Dog" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01521"]
    }

policeBadge2 :: CardDef
policeBadge2 =
  (asset "01027" "Police Badge" 3 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item]
    , cdLevel = 2
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01527"]
    }

beatCop2 :: CardDef
beatCop2 =
  (asset "01028" "Beat Cop" 4 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Ally, Police]
    , cdLevel = 2
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01528"]
    }

shotgun4 :: CardDef
shotgun4 =
  (asset "01029" "Shotgun" 5 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdLevel = 4
    , cdUses = uses Ammo 2
    , cdSlots = [#hand, #hand]
    , cdAlternateCardCodes = ["01529"]
    }

magnifyingGlass :: CardDef
magnifyingGlass =
  fast
    $ (asset "01030" "Magnifying Glass" 1 Seeker)
      { cdSkills = [#intellect]
      , cdCardTraits = setFromList [Item, Tool]
      , cdSlots = [#hand]
      , cdAlternateCardCodes = ["01530"]
      }

oldBookOfLore :: CardDef
oldBookOfLore =
  (asset "01031" "Old Book of Lore" 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01531"]
    }

researchLibrarian :: CardDef
researchLibrarian =
  (asset "01032" "Research Librarian" 2 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01532"]
    }

drMilanChristopher :: CardDef
drMilanChristopher =
  (asset "01033" ("Dr. Milan Christopher" <:> "Professor of Entomology") 4 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01533"]
    }

hyperawareness :: CardDef
hyperawareness =
  (asset "01034" "Hyperawareness" 2 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01534"]
    }

medicalTexts :: CardDef
medicalTexts =
  (asset "01035" "Medical Texts" 2 Seeker)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01535"]
    }

magnifyingGlass1 :: CardDef
magnifyingGlass1 =
  fast
    $ (asset "01040" "Magnifying Glass" 0 Seeker)
      { cdSkills = [#intellect]
      , cdCardTraits = setFromList [Item, Tool]
      , cdLevel = 1
      , cdSlots = [#hand]
      , cdAlternateCardCodes = ["01540"]
      }

discOfItzamna2 :: CardDef
discOfItzamna2 =
  (asset "01041" ("Disc of Itzamna" <:> "Protective Amulet") 3 Seeker)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 2
    , cdUnique = True
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01541"]
    }

encyclopedia2 :: CardDef
encyclopedia2 =
  (asset "01042" "Encyclopedia" 2 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Tome]
    , cdLevel = 2
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01542"]
    }

switchblade :: CardDef
switchblade =
  fast
    $ (asset "01044" "Switchblade" 1 Rogue)
      { cdSkills = [#agility]
      , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
      , cdSlots = [#hand]
      , cdAlternateCardCodes = ["01544", "60307"]
      }

burglary :: CardDef
burglary =
  (asset "01045" "Burglary" 1 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Talent, Illicit]
    , cdAlternateCardCodes = ["01545"]
    }

pickpocketing :: CardDef
pickpocketing =
  (asset "01046" "Pickpocketing" 2 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent, Illicit]
    , cdAlternateCardCodes = ["01546"]
    }

fortyOneDerringer :: CardDef
fortyOneDerringer =
  (asset "01047" ".41 Derringer" 3 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01547"]
    }

leoDeLuca :: CardDef
leoDeLuca =
  (asset "01048" ("Leo De Luca" <:> "The Louisiana Lion") 6 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01548"]
    }

hardKnocks :: CardDef
hardKnocks =
  (asset "01049" "Hard Knocks" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01549"]
    }

leoDeLuca1 :: CardDef
leoDeLuca1 =
  (asset "01054" ("Leo De Luca" <:> "The Louisiana Lion") 5 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdLevel = 1
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01554"]
    }

catBurglar1 :: CardDef
catBurglar1 =
  (asset "01055" "Cat Burglar" 4 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdLevel = 1
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01555"]
    }

forbiddenKnowledge :: CardDef
forbiddenKnowledge =
  (asset "01058" "Forbidden Knowledge" 0 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Secret 4
    , cdAlternateCardCodes = ["01558"]
    }

holyRosary :: CardDef
holyRosary =
  (asset "01059" "Holy Rosary" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01559"]
    }

shrivelling :: CardDef
shrivelling =
  (asset "01060" "Shrivelling" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    , cdAlternateCardCodes = ["01560"]
    }

scrying :: CardDef
scrying =
  (asset "01061" "Scrying" 1 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdAlternateCardCodes = ["01561"]
    }

arcaneStudies :: CardDef
arcaneStudies =
  (asset "01062" "Arcane Studies" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01562"]
    }

arcaneInitiate :: CardDef
arcaneInitiate =
  (asset "01063" "Arcane Initiate" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01563"]
    }

bookOfShadows3 :: CardDef
bookOfShadows3 =
  (asset "01070" "Book of Shadows" 4 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdLevel = 3
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01570"]
    }

grotesqueStatue4 :: CardDef
grotesqueStatue4 =
  (asset "01071" "Grotesque Statue" 2 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 4
    , cdUses = uses Charge 4
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01571"]
    }

leatherCoat :: CardDef
leatherCoat =
  (asset "01072" "Leather Coat" 0 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Armor]
    , cdSlots = [#body]
    , cdAlternateCardCodes = ["01572"]
    }

scavenging :: CardDef
scavenging =
  (asset "01073" "Scavenging" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01573"]
    }

baseballBat :: CardDef
baseballBat =
  (asset "01074" "Baseball Bat" 2 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand, #hand]
    , cdAlternateCardCodes = ["01574"]
    }

rabbitsFoot :: CardDef
rabbitsFoot =
  (asset "01075" "Rabbit's Foot" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01575", "60510"]
    }

strayCat :: CardDef
strayCat =
  (asset "01076" "Stray Cat" 1 Survivor)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01576"]
    }

digDeep :: CardDef
digDeep =
  (asset "01077" "Dig Deep" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdAlternateCardCodes = ["01577"]
    }

aquinnah1 :: CardDef
aquinnah1 =
  (asset "01082" ("Aquinnah" <:> "The Forgotten Daughter") 5 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally]
    , cdLevel = 1
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01582"]
    }

knife :: CardDef
knife =
  (asset "01086" "Knife" 1 Neutral)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01586"]
    }

flashlight :: CardDef
flashlight =
  (asset "01087" "Flashlight" 2 Neutral)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdUses = uses Supply 3
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01587"]
    }

bulletproofVest3 :: CardDef
bulletproofVest3 =
  (asset "01094" "Bulletproof Vest" 2 Neutral)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Item, Armor]
    , cdLevel = 3
    , cdSlots = [#body]
    , cdAlternateCardCodes = ["01594"]
    }

elderSignAmulet3 :: CardDef
elderSignAmulet3 =
  (asset "01095" "Elder Sign Amulet" 2 Neutral)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 3
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["01595"]
    }

litaChantler :: CardDef
litaChantler =
  (storyAsset "01117" ("Lita Chantler" <:> "The Zealot") 0 TheGathering)
    { cdCardTraits = setFromList [Ally]
    , cdUnique = True
    , cdSlots = [#ally]
    }

zoeysCross :: CardDef
zoeysCross =
  (asset "02006" ("Zoey's Cross" <:> "Symbol of Righteousness") 1 Neutral)
    { cdSkills = [#combat, #combat, #wild]
    , cdCardTraits = setFromList [Item, Charm]
    , cdUnique = True
    , cdSlots = [#accessory]
    , cdDeckRestrictions = [Signature "02001"]
    }

jennysTwin45s :: CardDef
jennysTwin45s =
  (asset "02010" ("Jenny's Twin .45s" <:> "A Perfect Fit") 0 Neutral)
    { cdSkills = [#agility, #agility, #wild]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdCost = Just DynamicCost
    , cdUnique = True
    , cdSlots = [#hand, #hand]
    , cdDeckRestrictions = [Signature "02003"]
    }

jimsTrumpet :: CardDef
jimsTrumpet =
  (asset "02012" ("Jim's Trumpet" <:> "The Dead Listen") 2 Neutral)
    { cdSkills = [#willpower, #willpower, #wild]
    , cdCardTraits = setFromList [Item, Instrument, Relic]
    , cdUnique = True
    , cdSlots = [#hand]
    , cdDeckRestrictions = [Signature "02004"]
    }

duke :: CardDef
duke =
  (asset "02014" ("Duke" <:> "Loyal Hound") 2 Neutral)
    { cdCardTraits = setFromList [Ally, Creature]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "02005"]
    }

blackjack :: CardDef
blackjack =
  (asset "02016" "Blackjack" 1 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand]
    }

laboratoryAssistant :: CardDef
laboratoryAssistant =
  (asset "02020" "Laboratory Assistant" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic, Science]
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["60212"]
    }

strangeSolution :: CardDef
strangeSolution =
  (asset "02021" ("Strange Solution" <:> "Unidentified") 1 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Science]
    }

liquidCourage :: CardDef
liquidCourage =
  (asset "02024" "Liquid Courage" 1 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 4
    }

hiredMuscle1 :: CardDef
hiredMuscle1 =
  (asset "02027" "Hired Muscle" 1 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdLevel = 1
    , cdSlots = [#ally]
    }

riteOfSeeking :: CardDef
riteOfSeeking =
  (asset "02028" "Rite of Seeking" 4 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

ritualCandles :: CardDef
ritualCandles =
  (asset "02029" "Ritual Candles" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Item
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["60405"]
    }

clarityOfMind :: CardDef
clarityOfMind =
  (asset "02030" "Clarity of Mind" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

fireAxe :: CardDef
fireAxe =
  (asset "02032" "Fire Axe" 1 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

peterSylvestre :: CardDef
peterSylvestre =
  (asset "02033" ("Peter Sylvestre" <:> "Big Man on Campus") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

peterSylvestre2 :: CardDef
peterSylvestre2 =
  (asset "02035" ("Peter Sylvestre" <:> "Big Man on Campus") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdLevel = 2
    , cdUnique = True
    , cdSlots = [#ally]
    }

kukri :: CardDef
kukri =
  (asset "02036" "Kukri" 2 Neutral)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

drHenryArmitage :: CardDef
drHenryArmitage =
  (storyAsset "02040" ("Dr. Henry Armitage" <:> "The Head Librarian") 2 ArmitagesFate)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

alchemicalConcoction :: CardDef
alchemicalConcoction =
  (storyAsset "02059" "Alchemical Concoction" 0 ExtracurricularActivity)
    { cdCardTraits = setFromList [Item, Science]
    , cdCardType = EncounterAssetType
    }

jazzMulligan :: CardDef
jazzMulligan =
  (storyAsset "02060" ("\"Jazz\" Mulligan" <:> "The Head Janitor") 0 ExtracurricularActivity)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

professorWarrenRice :: CardDef
professorWarrenRice =
  ( storyAsset "02061" ("Professor Warren Rice" <:> "Professor of Languages") 3 ExtracurricularActivity
  )
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

peterClover :: CardDef
peterClover =
  (storyAsset "02079" ("Peter Clover" <:> "Holding All the Cards") 0 TheHouseAlwaysWins)
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

drFrancisMorgan :: CardDef
drFrancisMorgan =
  (storyAsset "02080" ("Dr. Francis Morgan" <:> "Professor of Archaeology") 3 TheHouseAlwaysWins)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

brotherXavier1 :: CardDef
brotherXavier1 =
  (asset "02106" ("Brother Xavier" <:> "Pure of Spirit") 5 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally]
    , cdLevel = 1
    , cdUnique = True
    , cdSlots = [#ally]
    }

pathfinder1 :: CardDef
pathfinder1 =
  (asset "02108" "Pathfinder" 3 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Talent
    , cdLevel = 1
    }

adaptable1 :: CardDef
adaptable1 =
  permanent
    $ (asset "02110" "Adaptable" 0 Rogue)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = 1
      }

songOfTheDead2 :: CardDef
songOfTheDead2 =
  (asset "02112" "Song of the Dead" 2 Mystic)
    { cdCardTraits = setFromList [Spell, Song]
    , cdSkills = [#willpower]
    , cdLevel = 2
    , cdUses = uses Charge 5
    , cdSlots = [#arcane]
    }

fireExtinguisher1 :: CardDef
fireExtinguisher1 =
  (asset "02114" "Fire Extinguisher" 2 Survivor)
    { cdCardTraits = setFromList [Item, Tool, Melee]
    , cdSkills = [#agility]
    , cdLevel = 1
    , cdSlots = [#hand]
    }

smokingPipe :: CardDef
smokingPipe =
  (asset "02116" "Smoking Pipe" 1 Neutral)
    { cdCardTraits = singleton Item
    , cdSkills = [#willpower]
    , cdUses = uses Supply 3
    }

painkillers :: CardDef
painkillers =
  (asset "02117" "Painkillers" 1 Neutral)
    { cdCardTraits = singleton Item
    , cdSkills = [#willpower]
    , cdUses = uses Supply 3
    }

haroldWalsted :: CardDef
haroldWalsted =
  (storyAsset "02138" ("Harold Walsted" <:> "Curator of the Museum") 0 TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

adamLynch :: CardDef
adamLynch =
  (storyAsset "02139" ("Adam Lynch" <:> "Museum Security") 0 TheMiskatonicMuseum)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

theNecronomiconOlausWormiusTranslation :: CardDef
theNecronomiconOlausWormiusTranslation =
  (storyAsset "02140" ("The Necronomicon" <:> "Olaus Wormius Translation") 2 TheMiskatonicMuseum)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    }

bandolier :: CardDef
bandolier =
  (asset "02147" "Bandolier" 2 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item]
    , cdSlots = [#body]
    }

artStudent :: CardDef
artStudent =
  (asset "02149" "Art Student" 2 Seeker)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSkills = [#intellect]
    , cdSlots = [#ally]
    }

switchblade2 :: CardDef
switchblade2 =
  fast
    $ (asset "02152" "Switchblade" 1 Rogue)
      { cdSkills = [#combat, #agility]
      , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
      , cdLevel = 2
      , cdSlots = [#hand]
      }

shrivelling3 :: CardDef
shrivelling3 =
  (asset "02154" "Shrivelling" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdLevel = 3
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    }

newspaper :: CardDef
newspaper =
  (asset "02155" "Newspaper" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Item
    , cdSlots = [#hand]
    }

relicHunter3 :: CardDef
relicHunter3 =
  permanent
    $ (asset "02157" "Relic Hunter" 0 Neutral)
      { cdCardTraits = singleton Talent
      , cdLevel = 3
      , cdAlternateCardCodes = ["01695"]
      }

charisma3 :: CardDef
charisma3 =
  permanent
    $ (asset "02158" "Charisma" 0 Neutral)
      { cdCardTraits = singleton Talent
      , cdLevel = 3
      , cdAlternateCardCodes = ["01694"]
      }

helplessPassenger :: CardDef
helplessPassenger =
  (storyAsset "02179" "Helpless Passenger" 0 TheEssexCountyExpress)
    { cdCardTraits = setFromList [Ally, Bystander]
    , cdKeywords = singleton Keyword.Surge
    , cdEncounterSetQuantity = Just 3
    , cdCardType = EncounterAssetType
    }

keenEye3 :: CardDef
keenEye3 =
  permanent
    $ (asset "02185" "Keen Eye" 0 Guardian)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = 3
      }

higherEducation3 :: CardDef
higherEducation3 =
  permanent
    $ (asset "02187" "Higher Education" 0 Seeker)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = 3
      }

loneWolf :: CardDef
loneWolf =
  (asset "02188" "Lone Wolf" 1 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent]
    , cdLimits = [LimitPerInvestigator 1]
    }

streetwise3 :: CardDef
streetwise3 =
  permanent
    $ (asset "02189" "Streetwise" 0 Rogue)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = 3
      }

bloodPact3 :: CardDef
bloodPact3 =
  permanent
    $ (asset "02191" "Blood Pact" 0 Mystic)
      { cdCardTraits = setFromList [Spell, Pact]
      , cdLevel = 3
      }

scrapper3 :: CardDef
scrapper3 =
  permanent
    $ (asset "02193" "Scrapper" 0 Survivor)
      { cdCardTraits = setFromList [Talent]
      , cdLevel = 3
      }

keyToTheChamber :: CardDef
keyToTheChamber =
  (storyAsset "02215" "Key to the Chamber" 0 BloodOnTheAltar)
    { cdCardTraits = setFromList [Item, Key]
    , cdUnique = True
    , cdCardType = EncounterAssetType
    }

zebulonWhateley :: CardDef
zebulonWhateley =
  (storyAsset "02217" ("Zebulon Whateley" <:> "Recalling Ancient Things") 3 BloodOnTheAltar)
    { cdCardTraits = setFromList [Ally, Dunwich]
    , cdSkills = [#willpower, #wild]
    , cdUnique = True
    , cdSlots = [#ally]
    }

earlSawyer :: CardDef
earlSawyer =
  (storyAsset "02218" ("Earl Sawyer" <:> "Smarter Than He Lets On") 3 BloodOnTheAltar)
    { cdCardTraits = setFromList [Ally, Dunwich]
    , cdSkills = [#agility, #wild]
    , cdUnique = True
    , cdSlots = [#ally]
    }

powderOfIbnGhazi :: CardDef
powderOfIbnGhazi =
  (storyAsset "02219" ("Powder of Ibn-Ghazi" <:> "Seeing Things Unseen") 0 BloodOnTheAltar)
    { cdCardTraits = singleton Item
    }

springfieldM19034 :: CardDef
springfieldM19034 =
  (asset "02226" "Springfield M1903" 4 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdLevel = 4
    , cdSkills = [#combat, #agility]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand, #hand]
    }

luckyDice2 :: CardDef
luckyDice2 =
  (asset "02230" ("Lucky Dice" <:> "...Or Are They?") 2 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#willpower, #agility]
    , cdExceptional = True
    , cdLevel = 2
    , cdSlots = [#accessory]
    }

alyssaGraham :: CardDef
alyssaGraham =
  (asset "02232" ("Alyssa Graham" <:> "Speaker to the Dead") 4 Mystic)
    { cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSkills = [#intellect]
    , cdUnique = True
    , cdSlots = [#ally]
    }

riteOfSeeking4 :: CardDef
riteOfSeeking4 =
  (asset "02233" "Rite of Seeking" 5 Mystic)
    { cdCardTraits = singleton Spell
    , cdSkills = [#intellect, #intellect]
    , cdLevel = 4
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

darkHorse :: CardDef
darkHorse =
  (asset "02234" "Dark Horse" 3 Survivor)
    { cdCardTraits = singleton Condition
    , cdSkills = [#willpower]
    , cdLimits = [LimitPerInvestigator 1]
    }

esotericFormula :: CardDef
esotericFormula =
  (storyAsset "02254" "Esoteric Formula" 0 UndimensionedAndUnseen)
    { cdCardTraits = singleton Spell
    , cdEncounterSetQuantity = Just 4
    }

strangeSolutionRestorativeConcoction4 :: CardDef
strangeSolutionRestorativeConcoction4 =
  (asset "02262" ("Strange Solution" <:> "Restorative Concoction") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#willpower, #willpower]
    , cdLevel = 4
    , cdUses = uses Supply 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

strangeSolutionAcidicIchor4 :: CardDef
strangeSolutionAcidicIchor4 =
  (asset "02263" ("Strange Solution" <:> "Acidic Ichor") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#combat, #combat]
    , cdLevel = 4
    , cdUses = uses Supply 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

strangeSolutionFreezingVariant4 :: CardDef
strangeSolutionFreezingVariant4 =
  (asset "02264" ("Strange Solution" <:> "Freezing Variant") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#agility, #agility]
    , cdLevel = 4
    , cdUses = uses Supply 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

joeyTheRatVigil :: CardDef
joeyTheRatVigil =
  (asset "02265" ("Joey \"The Rat\" Vigil" <:> "Lookin' Out for #1") 4 Rogue)
    { cdCardTraits = setFromList [Ally, Criminal]
    , cdSkills = [#intellect, #agility]
    , cdUnique = True
    , cdSlots = [#ally]
    }

jewelOfAureolus3 :: CardDef
jewelOfAureolus3 =
  (asset "02269" ("Jewel of Aureolus" <:> "Gift of the Homunculi") 3 Mystic)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#wild]
    , cdLevel = 3
    , cdUnique = True
    , cdSlots = [#accessory]
    }

fineClothes :: CardDef
fineClothes =
  (asset "02272" "Fine Clothes" 1 Neutral)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdSkills = [#agility]
    , cdSlots = [#body]
    }

lightningGun5 :: CardDef
lightningGun5 =
  (asset "02301" "Lightning Gun" 6 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdLevel = 5
    , cdSkills = [#intellect, #combat]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand, #hand]
    }

drWilliamTMaleson :: CardDef
drWilliamTMaleson =
  (asset "02302" ("Dr. William T. Maleson" <:> "Working on Something Big") 1 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

chicagoTypewriter4 :: CardDef
chicagoTypewriter4 =
  (asset "02304" "Chicago Typewriter" 5 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdLevel = 4
    , cdUses = uses Ammo 4
    , cdSlots = [#hand, #hand]
    }

theGoldPocketWatch4 :: CardDef
theGoldPocketWatch4 =
  (asset "02305" ("The Gold Pocket Watch" <:> "Stealing Time") 2 Rogue)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 4
    , cdUnique = True
    , cdExceptional = True
    , cdSlots = [#accessory]
    }

shrivelling5 :: CardDef
shrivelling5 =
  (asset "02306" "Shrivelling" 3 Mystic)
    { cdSkills = [#willpower, #combat, #combat]
    , cdCardTraits = singleton Spell
    , cdLevel = 5
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    }

aquinnah3 :: CardDef
aquinnah3 =
  (asset "02308" ("Aquinnah" <:> "The Forgotten Daughter") 4 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Ally]
    , cdLevel = 3
    , cdUnique = True
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["01691"]
    }

tryAndTryAgain3 :: CardDef
tryAndTryAgain3 =
  (asset "02309" "Try and Try Again" 2 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Talent
    , cdLevel = 3
    }

theRedGlovedMan5 :: CardDef
theRedGlovedMan5 =
  fast
    $ (asset "02310" ("The Red-Gloved Man" <:> "He Was Never There") 2 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Ally, Conspirator]
      , cdLevel = 5
      , cdUnique = True
      , cdSlots = [#ally]
      }

sophieInLovingMemory :: CardDef
sophieInLovingMemory =
  (asset "03009" ("Sophie" <:> "In Loving Memory") 0 Neutral)
    { cdCardTraits = setFromList [Item, Spirit]
    , cdUnique = True
    , cdCost = Nothing
    , cdDeckRestrictions = [Signature "03001"]
    }

sophieItWasAllMyFault :: CardDef
sophieItWasAllMyFault =
  (asset "03009b" ("Sophie" <:> "It Was All My Fault") 0 Neutral)
    { cdCardTraits = setFromList [Item, Madness]
    , cdUnique = True
    , cdCost = Nothing
    , cdDeckRestrictions = [Signature "03001"]
    }

analyticalMind :: CardDef
analyticalMind =
  (asset "03010" ("Analytical Mind" <:> "Between the Lines") 3 Neutral)
    { cdCardTraits = singleton Talent
    , cdSkills = [#wild, #wild]
    , cdDeckRestrictions = [Signature "03002"]
    }

theKingInYellow :: CardDef
theKingInYellow =
  (weakness "03011" ("The King in Yellow" <:> "Act 1"))
    { cdCardTraits = singleton Tome
    , cdUnique = True
    , cdSlots = [#hand]
    }

spiritSpeaker :: CardDef
spiritSpeaker =
  (asset "03014" ("Spirit-Speaker" <:> "Envoy of the Alusi") 2 Neutral)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = singleton Ritual
    , cdDeckRestrictions = [Signature "03004"]
    }

thirtyTwoColt :: CardDef
thirtyTwoColt =
  (asset "03020" ".32 Colt" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 6
    , cdSlots = [#hand]
    }

trueGrit :: CardDef
trueGrit =
  (asset "03021" "True Grit" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

fieldwork :: CardDef
fieldwork =
  (asset "03024" "Fieldwork" 2 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Talent
    }

archaicGlyphs :: CardDef
archaicGlyphs =
  (asset "03025" ("Archaic Glyphs" <:> "Untranslated") 0 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Occult, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 0
    }

inTheKnow1 :: CardDef
inTheKnow1 =
  (asset "03027" "In the Know" 3 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Talent
    , cdUses = uses Secret 3
    , cdLevel = 1
    }

stealth :: CardDef
stealth =
  (asset "03028" "Stealth" 2 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Talent
    }

lockpicks1 :: CardDef
lockpicks1 =
  (asset "03031" "Lockpicks" 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool, Illicit]
    , cdUses = uses Supply 3
    , cdLevel = 1
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01687"]
    }

alchemicalTransmutation :: CardDef
alchemicalTransmutation =
  (asset "03032" "Alchemical Transmutation" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    }

spiritAthame1 :: CardDef
spiritAthame1 =
  (asset "03035" "Spirit Athame" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdLevel = 1
    , cdSlots = [#hand]
    }

lantern :: CardDef
lantern =
  (asset "03036" "Lantern" 2 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    }

gravediggersShovel :: CardDef
gravediggersShovel =
  (asset "03037" "Gravedigger's Shovel" 2 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSlots = [#hand]
    }

constanceDumaine :: CardDef
constanceDumaine =
  (storyAsset "03076a" ("Constance Dumaine" <:> "Sociable Hostess") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

jordanPerry :: CardDef
jordanPerry =
  (storyAsset "03077" ("Jordan Perry" <:> "Dignified Financier") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

ishimaruHaruko :: CardDef
ishimaruHaruko =
  (storyAsset "03078" ("Ishimaru Haruko" <:> "Costume Designer") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

sebastienMoreau :: CardDef
sebastienMoreau =
  (storyAsset "03079" ("Sebastien Moreau" <:> "Impassioned Producer") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

ashleighClarke :: CardDef
ashleighClarke =
  (storyAsset "03080" ("Ashleigh Clarke" <:> "Talented Entertainer") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    , cdCost = Nothing
    }

combatTraining1 :: CardDef
combatTraining1 =
  (asset "03107" "Combat Training" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = 1
    }

scientificTheory1 :: CardDef
scientificTheory1 =
  (asset "03109" "Scientific Theory" 1 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = 1
    }

knuckleduster :: CardDef
knuckleduster =
  (asset "03110" "Knuckleduster" 2 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
    , cdSlots = [#hand]
    }

moxie1 :: CardDef
moxie1 =
  (asset "03111" "Moxie" 1 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = 1
    }

davidRenfield :: CardDef
davidRenfield =
  (asset "03112" ("David Renfield" <:> "Esteemed Eschatologist") 2 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

grounded1 :: CardDef
grounded1 =
  (asset "03113" "Grounded" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = 1
    }

cherishedKeepsake :: CardDef
cherishedKeepsake =
  (asset "03114" "Cherished Keepsake" 0 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    }

plucky1 :: CardDef
plucky1 =
  (asset "03115" "Plucky" 1 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent, Composure]
    , cdLimits = [LimitPerTrait Composure 1]
    , cdLevel = 1
    }

mrPeabody :: CardDef
mrPeabody =
  (storyAsset "03141" ("Mr. Peabody" <:> "Historical Society Curator") 0 EchoesOfThePast)
    { cdCardTraits = setFromList [Ally, HistoricalSociety]
    , cdCost = Nothing
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdSlots = [#ally]
    }

claspOfBlackOnyx :: CardDef
claspOfBlackOnyx =
  (storyWeakness "03142" ("Clasp of Black Onyx" <:> "A Gift Unlooked For") EchoesOfThePast)
    { cdCardTraits = setFromList [Item, Relic]
    , cdCost = Just (StaticCost 1)
    , cdRevelation = NoRevelation
    , cdCardInHandEffects = True
    }

theTatteredCloak :: CardDef
theTatteredCloak =
  (storyAsset "03143" ("The Tattered Cloak" <:> "Regalia Dementia") 2 EchoesOfThePast)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    }

trenchKnife :: CardDef
trenchKnife =
  (asset "03147" "Trench Knife" 1 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

charlesRossEsq :: CardDef
charlesRossEsq =
  (asset "03149" ("Charles Ross, Esq." <:> "Acquisitions and Solicitation") 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

darioElAmin :: CardDef
darioElAmin =
  (asset "03151" ("Dario El-Amin" <:> "Unscrupulous Investor") 4 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

bookOfShadows1 :: CardDef
bookOfShadows1 =
  (asset "03154" "Book of Shadows" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdLevel = 1
    , cdSlots = [#hand]
    }

danielChesterfield :: CardDef
danielChesterfield =
  ( storyAsset "03182a" ("Daniel Chesterfield" <:> "He's Not Doing All Too Well") 0 TheUnspeakableOath
  )
    { cdCardTraits = setFromList [Ally, Lunatic]
    , cdCost = Nothing
    , cdUnique = True
    , cdCardType = EncounterAssetType
    , cdDoubleSided = True
    }

straitjacket :: CardDef
straitjacket =
  (storyAsset "x03185" "Straitjacket" 0 TheUnspeakableOath)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdCardType = EncounterAssetType
    , cdSlots = [#body, #hand, #hand]
    , cdEncounterSetQuantity = Just 2
    , cdCost = Nothing
    , cdClassSymbols = singleton Mythos
    }

fortyFiveAutomatic2 :: CardDef
fortyFiveAutomatic2 =
  (asset "03190" ".45 Automatic" 4 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 4
    , cdLevel = 2
    }

archaicGlyphsGuidingStones3 :: CardDef
archaicGlyphsGuidingStones3 =
  (asset "03192" ("Archaic Glyphs" <:> "Guiding Stones") 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGlyphs
    }

archaicGlyphsProphecyForetold3 :: CardDef
archaicGlyphsProphecyForetold3 =
  (asset "03193" ("Archaic Glyphs" <:> "Prophecy Foretold") 2 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGlyphs
    }

pickpocketing2 :: CardDef
pickpocketing2 =
  fast
    $ (asset "03195" "Pickpocketing" 2 Rogue)
      { cdSkills = [#agility, #agility]
      , cdCardTraits = setFromList [Talent, Illicit]
      , cdLevel = 2
      }

madameLabranche :: CardDef
madameLabranche =
  (asset "03198" ("Madame Labranche" <:> "Mysterious Benefactress") 2 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }

firstAid3 :: CardDef
firstAid3 =
  (asset "03230" "First Aid" 2 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Talent, Science]
    , cdUses = uses Supply 4
    , cdLevel = 3
    , cdAlternateCardCodes = ["01683"]
    }

fortyOneDerringer2 :: CardDef
fortyOneDerringer2 =
  (asset "03234" ".41 Derringer" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    , cdLevel = 2
    , cdAlternateCardCodes = ["01688"]
    }

scrying3 :: CardDef
scrying3 =
  (asset "03236" "Scrying" 1 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = 3
    , cdAlternateCardCodes = ["01690"]
    }

stickToThePlan3 :: CardDef
stickToThePlan3 =
  permanent
    $ (asset "03264" "Stick to the Plan" 0 Guardian)
      { cdCardTraits = singleton Talent
      , cdKeywords = setFromList [Keyword.Permanent, Keyword.Exceptional]
      , cdLevel = 3
      }

arcaneInsight4 :: CardDef
arcaneInsight4 =
  (asset "03266" "Arcane Insight" 3 Seeker)
    { cdCardTraits = singleton Spell
    , cdSkills = [#willpower, #intellect]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = 4
    }

suggestion4 :: CardDef
suggestion4 =
  (asset "03268" "Suggestion" 3 Rogue)
    { cdCardTraits = singleton Spell
    , cdSkills = [#willpower, #agility]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = 4
    }

stHubertsKey :: CardDef
stHubertsKey =
  (asset "03269" ("St. Hubert's Key" <:> "Cleansing Fire") 4 Mystic)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSkills = [#willpower]
    , cdSlots = [#accessory]
    , cdUnique = True
    }

arcaneInitiate3 :: CardDef
arcaneInitiate3 =
  (asset "03271" "Arcane Initiate" 0 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSlots = [#ally]
    , cdLevel = 3
    }

armorOfArdennes5 :: CardDef
armorOfArdennes5 =
  (asset "03305" "Armor of Ardennes" 4 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Item, Armor, Relic]
    , cdSlots = [#body]
    , cdLevel = 5
    }

charonsObol1 :: CardDef
charonsObol1 =
  permanent
    $ (asset "03308" ("Charon's Obol" <:> "The Ferryman's Pay") 0 Rogue)
      { cdCardTraits = setFromList [Item, Relic]
      , cdLevel = 1
      , cdKeywords = setFromList [Keyword.Permanent, Keyword.Exceptional]
      , cdUnique = True
      }

lupara3 :: CardDef
lupara3 =
  (asset "03309" "Lupara" 3 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdLevel = 3
    , cdUses = uses Ammo 2
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdSlots = [#hand]
    }

newspaper2 :: CardDef
newspaper2 =
  (asset "03313" "Newspaper" 1 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Item
    , cdLevel = 2
    , cdSlots = [#hand]
    }

keyOfYs :: CardDef
keyOfYs =
  (asset "03315" ("Key of Ys" <:> "Let the Storm Rage") 3 Neutral)
    { cdSkills = [#wild, #willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 5
    , cdSlots = [#accessory]
    , cdUnique = True
    }

thePallidMask :: CardDef
thePallidMask =
  (asset "03321b" ("The Pallid Mask" <:> "Chasing Tails") 0 Neutral)
    { cdCardTraits = setFromList [Item, Relic]
    , cdRevelation = IsRevelation
    , cdUnique = True
    }

mitchBrown :: CardDef
mitchBrown =
  (asset "04006" ("Mitch Brown" <:> "Sole Survivor") 3 Neutral)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "04001"]
    }

jakeWilliams :: CardDef
jakeWilliams =
  (asset "04008" ("Jake Williams" <:> "Loyal Companion") 3 Neutral)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "04002"]
    }

finnsTrustyThirtyEight :: CardDef
finnsTrustyThirtyEight =
  fast
    $ (asset "04011" ("Finn's Trusty .38" <:> "Never Leave Home Without It") 2 Neutral)
      { cdSkills = [#agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
      , cdSlots = [#hand]
      , cdUses = uses Ammo 3
      , cdUnique = True
      , cdDeckRestrictions = [Signature "04003"]
      }

theCodexOfAges :: CardDef
theCodexOfAges =
  (asset "04013" ("The Codex of Ages" <:> "finis omnium nunc est") 2 Neutral)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome, Blessed]
    , cdSlots = [#hand]
    , cdKeywords = singleton (seal Token.ElderSign)
    , cdUnique = True
    , cdDeckRestrictions = [Signature "04004"]
    }

untilTheEndOfTime :: CardDef
untilTheEndOfTime =
  (asset "04015" "Until the End of Time" 1 Neutral)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = singleton Talent
    , cdDeckRestrictions = [Signature "04005"]
    }

survivalKnife :: CardDef
survivalKnife =
  (asset "04017" "Survival Knife" 2 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

venturer :: CardDef
venturer =
  (asset "04018" "Venturer" 4 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUses = uses Supply 3
    }

drElliHorowitz :: CardDef
drElliHorowitz =
  (asset "04021" ("Dr. Elli Horowitz" <:> "Assistant Curator") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Assistant]
    , cdSlots = [#ally]
    , cdUnique = True
    }

ancientStone1 :: CardDef
ancientStone1 =
  (asset "04022" ("Ancient Stone" <:> "Unidentified") 1 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#hand]
    , cdLevel = 1
    }

toothOfEztli :: CardDef
toothOfEztli =
  (asset "04023" ("Tooth of Eztli" <:> "Mortal Reminder") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    }

treasureHunter1 :: CardDef
treasureHunter1 =
  (asset "04025" "Treasure Hunter" 1 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdLevel = 1
    }

decoratedSkull :: CardDef
decoratedSkull =
  (asset "04026" ("Decorated Skull" <:> "Doom Begets Doom") 0 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#accessory]
    , cdUses = uses Charge 0
    }

mistsOfRlyeh :: CardDef
mistsOfRlyeh =
  (asset "04029" "Mists of R'lyeh" 2 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

theChthonianStone :: CardDef
theChthonianStone =
  (asset "04030" ("The Chthonian Stone" <:> "Stygian Waymark") 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdKeywords =
        singleton
          $ seal
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Token.Skull, Token.Cultist, Token.Tablet, Token.ElderThing]
    }

protectiveIncantation1 :: CardDef
protectiveIncantation1 =
  (asset "04031" "Protective Incantation" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual, Blessed]
    , cdSlots = [#arcane]
    , cdKeywords = singleton (seal $ ChaosTokenFaceIsNot Token.AutoFail)
    , cdLevel = 1
    }

yaotl1 :: CardDef
yaotl1 =
  (asset "04035" ("Yaotl" <:> "Lost Son of Eztli") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = 1
    }

backpack :: CardDef
backpack =
  (asset "04037" "Backpack" 2 Neutral)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Item
    , cdSlots = [#body]
    }

alejandroVela :: CardDef
alejandroVela =
  (storyAsset "04051" ("Alejandro Vela" <:> "Renowned Historian") 2 TheUntamedWilds)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

relicOfAgesADeviceOfSomeSort :: CardDef
relicOfAgesADeviceOfSomeSort =
  (storyAsset "04061" ("Relic of Ages" <:> "\8230A Device, of Some Sort") 2 TheDoomOfEztli)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

shrewdAnalysis :: CardDef
shrewdAnalysis =
  permanent
    $ (asset "04106" "Shrewd Analysis" 0 Seeker)
      { cdCardTraits = singleton Talent
      }

luckyCigaretteCase :: CardDef
luckyCigaretteCase =
  (asset "04107" "Lucky Cigarette Case" 2 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdAlternateCardCodes = ["60308"]
    }

fence1 :: CardDef
fence1 =
  (asset "04108" "Fence" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Connection, Illicit]
    , cdLevel = 1
    }

arcaneResearch :: CardDef
arcaneResearch =
  permanent
    $ (asset "04109" "Arcane Research" 0 Mystic)
      { cdCardTraits = singleton Talent
      , cdPurchaseTrauma = PurchaseMentalTrauma 1
      }

harlanEarnstone :: CardDef
harlanEarnstone =
  (storyAsset "04118b" ("Harlan Earnstone" <:> "Historical Theorist") 0 ThreadsOfFate)
    { cdCardTraits = setFromList [Bystander, Miskatonic]
    , cdCost = Nothing
    , cdUnique = True
    }

henryDeveau :: CardDef
henryDeveau =
  (storyAsset "04125b" ("Henry Deveau" <:> "Friend of Alejandro") 0 ThreadsOfFate)
    { cdCardTraits = singleton Bystander
    , cdCost = Nothing
    , cdUnique = True
    }

mariaDeSilva :: CardDef
mariaDeSilva =
  (storyAsset "04134b" ("Maria DeSilva" <:> "Wealthy Patron") 0 ThreadsOfFate)
    { cdCardTraits = singleton Bystander
    , cdCost = Nothing
    , cdUnique = True
    }

ichtacaTheForgottenGuardian :: CardDef
ichtacaTheForgottenGuardian =
  (storyAsset "04147" ("Ichtaca" <:> "The Forgotten Guardian") 4 ThreadsOfFate)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Eztli, Wayfarer]
    , cdUnique = True
    , cdSlots = [#ally]
    }

expeditionJournal :: CardDef
expeditionJournal =
  (storyAsset "04148" "Expedition Journal" 2 ThreadsOfFate)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUnique = True
    }

wellPrepared2 :: CardDef
wellPrepared2 =
  (asset "04151" "Well Prepared" 2 Guardian)
    { cdCardTraits = singleton Talent
    , cdLevel = 2
    }

quickStudy2 :: CardDef
quickStudy2 =
  (asset "04154" "Quick Study" 2 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = 2
    }

highRoller2 :: CardDef
highRoller2 =
  (asset "04156" "High Roller" 2 Rogue)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Talent
    , cdLevel = 2
    }

recallTheFuture2 :: CardDef
recallTheFuture2 =
  (asset "04158" "Recall the Future" 2 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Augury, Ritual]
    , cdLevel = 2
    }

tryAndTryAgain1 :: CardDef
tryAndTryAgain1 =
  (asset "04159" "Try and Try Again" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    , cdUses = uses Try 3
    , cdLevel = 1
    }

cornered2 :: CardDef
cornered2 =
  (asset "04160" "Cornered" 2 Survivor)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Talent
    , cdLevel = 2
    }

relicOfAgesForestallingTheFuture :: CardDef
relicOfAgesForestallingTheFuture =
  (storyAsset "04191" ("Relic of Ages" <:> "Forestalling the Future") 2 TheBoundaryBeyond)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

otherworldlyCompass2 :: CardDef
otherworldlyCompass2 =
  (asset "04194" "Otherworldly Compass" 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    , cdLevel = 2
    }

lolaSantiago3 :: CardDef
lolaSantiago3 =
  (asset "04196" ("Lola Santiago" <:> "No-Nonsense Archaeologist") 3 Rogue)
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#ally]
    , cdLevel = 3
    , cdUnique = True
    }

oliveMcBride :: CardDef
oliveMcBride =
  (asset "04197" ("Olive McBride" <:> "Will Try Anything Once") 2 Mystic)
    { cdCardTraits = setFromList [Ally, Witch]
    , cdSkills = [#willpower]
    , cdSlots = [#ally]
    , cdUnique = True
    }

trenchCoat :: CardDef
trenchCoat =
  (asset "04203" "Trench Coat" 3 Neutral)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdSkills = [#agility]
    , cdSlots = [#body]
    }

ornateBow3 :: CardDef
ornateBow3 =
  (asset "04204" "Ornate Bow" 4 Neutral)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Ranged]
    , cdSkills = [#combat, #agility]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 1
    , cdLevel = 3
    }

m1918Bar4 :: CardDef
m1918Bar4 =
  (asset "04229" "M1918 BAR" 5 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#combat, #combat]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 8
    , cdLevel = 4
    }

ancientStoneKnowledgeOfTheElders4 :: CardDef
ancientStoneKnowledgeOfTheElders4 =
  (asset "04230" ("Ancient Stone" <:> "Knowledge of the Elders") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    , cdUses = uses Secret 0
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = 4
    }

ancientStoneMindsInHarmony4 :: CardDef
ancientStoneMindsInHarmony4 =
  (asset "04231" ("Ancient Stone" <:> "Minds in Harmony") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#willpower, #willpower]
    , cdSlots = [#hand]
    , cdUses = uses Secret 0
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = 4
    }

crystallineElderSign3 :: CardDef
crystallineElderSign3 =
  (asset "04235" "Crystalline Elder Sign" 3 Mystic)
    { cdCardTraits = setFromList [Item, Relic, Blessed]
    , cdSkills = [#wild]
    , cdSlots = [#accessory]
    , cdKeywords =
        singleton
          $ seal
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Token.PlusOne, Token.ElderSign]
    , cdLevel = 3
    }

onYourOwn3 :: CardDef
onYourOwn3 =
  (asset "04236" "On Your Own" 2 Survivor)
    { cdCardTraits = singleton Talent
    , cdSkills = [#willpower]
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = 3
    }

theCustodian :: CardDef
theCustodian =
  (storyAsset "04256" ("The Custodian" <:> "Curious Yithian") 0 TheCityOfArchives)
    { cdCardTraits = setFromList [Ally, Yithian]
    , cdUnique = True
    , cdCost = Nothing
    }

handcuffs :: CardDef
handcuffs =
  (asset "04265" "Handcuffs" 2 Guardian)
    { cdCardTraits = setFromList [Item, Police]
    , cdSkills = [#agility]
    }

feedTheMind3 :: CardDef
feedTheMind3 =
  (asset "04267" "Feed the Mind" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdUses = uses Secret 3
    , cdSlots = [#arcane]
    , cdLevel = 3
    }

coltVestPocket :: CardDef
coltVestPocket =
  (asset "04268" "Colt Vest Pocket" 2 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 5
    , cdSlots = [#hand]
    }

theSkeletonKey2 :: CardDef
theSkeletonKey2 =
  fast
    $ (asset "04270" "The Skeleton Key" 3 Rogue)
      { cdSkills = [#intellect, #intellect]
      , cdCardTraits = setFromList [Item, Relic, Cursed]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Exceptional]
      , cdLevel = 2
      }

mistsOfRlyeh4 :: CardDef
mistsOfRlyeh4 =
  (asset "04271" "Mists of R'lyeh" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 5
    , cdLevel = 4
    }

oldHuntingRifle3 :: CardDef
oldHuntingRifle3 =
  (asset "04273" "Old Hunting Rifle" 3 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 3
    , cdLevel = 3
    }

thermos :: CardDef
thermos =
  (asset "04274" "Thermos" 4 Neutral)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Item
    , cdUses = uses Supply 3
    }

hemisphericMap3 :: CardDef
hemisphericMap3 =
  (asset "04275" "Hemispheric Map" 2 Neutral)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    , cdLevel = 3
    }

timewornBrand5 :: CardDef
timewornBrand5 =
  (asset "04276" "Timeworn Brand" 5 Neutral)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = 5
    }

relicOfAgesRepossessThePast :: CardDef
relicOfAgesRepossessThePast =
  (storyAsset "04303" ("Relic of Ages" <:> "Repossess the Past") 2 TheDepthsOfYoth)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

kerosene1 :: CardDef
kerosene1 =
  (asset "04304" "Kerosene" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Item
    , cdUses = uses Supply 3
    , cdLevel = 1
    }

flamethrower5 :: CardDef
flamethrower5 =
  (asset "04305" "Flamethrower" 4 Guardian)
    { cdSkills = [#combat, #combat, #wild]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 4
    , cdSlots = [#body, #hand, #hand]
    , cdLevel = 5
    }

pnakoticManuscripts5 :: CardDef
pnakoticManuscripts5 =
  (asset "04307" ("Pnakotic Manuscripts" <:> "Mind-Expanding Ideas") 5 Seeker)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdUses = uses Secret 3
    , cdSlots = [#hand]
    , cdLevel = 5
    , cdUnique = True
    }

borrowedTime3 :: CardDef
borrowedTime3 =
  (asset "04308" "Borrowed Time" 1 Rogue)
    { cdSkills = [#willpower, #agility]
    , cdKeywords = singleton Keyword.Exceptional
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdLevel = 3
    }

shardsOfTheVoid3 :: CardDef
shardsOfTheVoid3 =
  (asset "04310" "Shards of the Void" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdKeywords = singleton $ seal Token.Zero
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = 3
    }

sealOfTheSeventhSign5 :: CardDef
sealOfTheSeventhSign5 =
  (asset "04311" ("Seal of the Seventh Sign" <:> "Over the Threshold and Beyond") 4 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdKeywords = singleton $ seal Token.AutoFail
    , cdCardTraits = setFromList [Spell, Ritual]
    , cdUses = uses Charge 7
    , cdSlots = [#arcane]
    , cdLevel = 5
    , cdUnique = True
    }

relicOfAgesUnleashTheTimestream :: CardDef
relicOfAgesUnleashTheTimestream =
  (storyAsset "04343" ("Relic of Ages" <:> "Unleash the Timestream") 2 ShatteredAeons)
    { cdSkills = [#wild, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

hypnoticTherapy :: CardDef
hypnoticTherapy =
  (asset "05007" "Hypnotic Therapy" 2 Neutral)
    { cdCardTraits = singleton Talent
    , cdSkills = [#willpower, #intellect, #wild]
    , cdDeckRestrictions = [Signature "05001"]
    }

detectivesColt1911s :: CardDef
detectivesColt1911s =
  (asset "05009" "Detective's Colt 1911s" 4 Neutral)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#intellect, #combat, #wild]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 4
    , cdDeckRestrictions = [Signature "05002"]
    }

familyInheritance :: CardDef
familyInheritance =
  permanent
    (asset "05011" ("Family Inheritance" <:> "A Windfall? Or a Burden?") 0 Neutral)
      { cdCardTraits = singleton Boon
      , cdUnique = True
      , cdDeckRestrictions = [Signature "05003"]
      }

twilightBlade :: CardDef
twilightBlade =
  (asset "05013" ("Twilight Blade" <:> "Sanctum's Reward") 3 Neutral)
    { cdCardTraits = setFromList [Item, Relic, Weapon]
    , cdSkills = [#willpower, #combat, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "05004"]
    }

baronSamedi :: CardDef
baronSamedi =
  (weakness "05019" ("Baron Samedi" <:> "Lord of the Cemetery"))
    { cdCardTraits = singleton Avatar
    , cdSlots = [#ally]
    , cdAlternateCardCodes = ["99003"]
    , cdUnique = True
    }

aceOfSwords1 :: CardDef
aceOfSwords1 =
  (asset "05023" ("Ace of Swords" <:> "Let Your Arrow Fly True") 3 Guardian)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 1
    , cdCardInHandEffects = True
    }

fingerprintKit :: CardDef
fingerprintKit =
  (asset "05024" "Fingerprint Kit" 4 Seeker)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    , cdUses = uses Supply 3
    }

deathXiii1 :: CardDef
deathXiii1 =
  (asset "05027" ("Death • XIII" <:> "Free from the Past") 3 Seeker)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 1
    , cdCardInHandEffects = True
    }

wellConnected :: CardDef
wellConnected =
  (asset "05028" "Well Connected" 2 Rogue)
    { cdCardTraits = singleton Condition
    , cdSkills = [#intellect]
    , cdLimits = [LimitPerInvestigator 1]
    }

theMoonXiii1 :: CardDef
theMoonXiii1 =
  (asset "05031" ("The Moon • XVIII" <:> "Message from Your Inner Self") 3 Rogue)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 1
    , cdCardInHandEffects = True
    }

fourOfCups1 :: CardDef
fourOfCups1 =
  (asset "05035" ("Four of Cups" <:> "Chalice of the Heart") 3 Mystic)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 1
    , cdCardInHandEffects = True
    }

trackShoes :: CardDef
trackShoes =
  (asset "05036" "Track Shoes" 3 Survivor)
    { cdCardTraits = setFromList [Item, Clothing, Footwear]
    , cdSkills = [#agility]
    , cdLimits = [LimitPerTrait Footwear 1]
    }

fiveOfPentacles1 :: CardDef
fiveOfPentacles1 =
  (asset "05039" ("Five of Pentacles" <:> "From the Brink") 3 Survivor)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 1
    , cdCardInHandEffects = True
    }

aceOfRods1 :: CardDef
aceOfRods1 =
  (asset "05040" ("Ace of Rods" <:> "The Fateful Step") 3 Neutral)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 1
    , cdCardInHandEffects = True
    }

theTowerXVI :: CardDef
theTowerXVI =
  (basicWeakness "05042" ("The Tower • XVI" <:> "Circumstances Beyond Your Control"))
    { cdCardTraits = setFromList [Omen, Tarot]
    , cdSlots = [#tarot]
    , cdCardInHandEffects = True
    , cdCanReplace = False
    , cdRevelation = NoRevelation
    , cdCost = Just (StaticCost 4)
    }

somethingWorthFightingFor :: CardDef
somethingWorthFightingFor =
  (asset "05109" "Something Worth Fighting For" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

signMagick :: CardDef
signMagick =
  fast
    (asset "05112" "Sign Magick" 3 Mystic)
      { cdSkills = [#willpower]
      , cdCardTraits = setFromList [Ritual, Talent]
      , cdSlots = [#hand]
      }

meatCleaver :: CardDef
meatCleaver =
  (asset "05114" "Meat Cleaver" 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    }

fortyFiveThompson :: CardDef
fortyFiveThompson =
  (multiClassAsset "05115" ".45 Thompson" 6 [Guardian, Rogue])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    }

scrollOfSecrets :: CardDef
scrollOfSecrets =
  (multiClassAsset "05116" "Scroll of Secrets" 1 [Seeker, Mystic])
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    }

tennesseeSourMash :: CardDef
tennesseeSourMash =
  (multiClassAsset "05117" "Tennessee Sour Mash" 3 [Rogue, Survivor])
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 2
    }

enchantedBlade :: CardDef
enchantedBlade =
  (multiClassAsset "05118" "Enchanted Blade" 3 [Mystic, Guardian])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdUses = uses Charge 3
    , cdSlots = [#hand, #arcane]
    }

grislyTotem :: CardDef
grislyTotem =
  (multiClassAsset "05119" "Grisly Totem" 3 [Survivor, Seeker])
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    }

theBlackBook :: CardDef
theBlackBook =
  (storyAsset "05150" ("The Black Book" <:> "Signed in Blood") 3 TheSecretName)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Item, Tome, Relic]
    , cdSlots = [#hand]
    , cdUnique = True
    }

aliceLuxley :: CardDef
aliceLuxley =
  (asset "05151" ("Alice Luxley" <:> "Fearless Flatfoot") 4 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Detective, Police]
    , cdSlots = [#ally]
    , cdUnique = True
    }

mrRook :: CardDef
mrRook =
  (asset "05153" ("Mr. \"Rook\"" <:> "Dealer in Secrets") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Secret 3
    }

hawkEyeFoldingCamera :: CardDef
hawkEyeFoldingCamera =
  (asset "05154" "Hawk-Eye Folding Camera" 2 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    }

henryWan :: CardDef
henryWan =
  (asset "05155" ("Henry Wan" <:> "Aspiring Actor") 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdSlots = [#ally]
    , cdUnique = True
    }

wither :: CardDef
wither =
  (asset "05157" "Wither" 2 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    }

sixthSense :: CardDef
sixthSense =
  (asset "05158" "Sixth Sense" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    }

drawingThin :: CardDef
drawingThin =
  (asset "05159" "Drawing Thin" 0 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

spectralWeb :: CardDef
spectralWeb =
  (storyAssetWithMany "05177" "Spectral Web" 0 TheWagesOfSin 4)
    { cdCardTraits = singleton Spell
    }

fortyFiveThompsonGuardian3 :: CardDef
fortyFiveThompsonGuardian3 =
  (asset "05186" ".45 Thompson" 6 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    , cdLevel = 3
    }

fortyFiveThompsonRogue3 :: CardDef
fortyFiveThompsonRogue3 =
  (asset "05187" ".45 Thompson" 5 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    , cdLevel = 3
    }

scrollOfSecretsSeeker3 :: CardDef
scrollOfSecretsSeeker3 =
  (asset "05188" "Scroll of Secrets" 1 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    , cdLevel = 3
    }

scrollOfSecretsMystic3 :: CardDef
scrollOfSecretsMystic3 =
  (asset "05189" "Scroll of Secrets" 1 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    , cdLevel = 3
    }

tennesseeSourMashRogue3 :: CardDef
tennesseeSourMashRogue3 =
  (asset "05190" "Tennessee Sour Mash" 3 Rogue)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 2
    , cdLevel = 3
    }

tennesseeSourMashSurvivor3 :: CardDef
tennesseeSourMashSurvivor3 =
  (asset "05191" "Tennessee Sour Mash" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 3
    , cdLevel = 3
    }

enchantedBladeGuardian3 :: CardDef
enchantedBladeGuardian3 =
  (asset "05192" "Enchanted Blade" 3 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdUses = uses Charge 3
    , cdSlots = [#hand, #arcane]
    , cdLevel = 3
    }

enchantedBladeMystic3 :: CardDef
enchantedBladeMystic3 =
  (asset "05193" "Enchanted Blade" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdUses = uses Charge 4
    , cdSlots = [#hand, #arcane]
    , cdLevel = 3
    }

grislyTotemSeeker3 :: CardDef
grislyTotemSeeker3 =
  (asset "05194" "Grisly Totem" 3 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    , cdLevel = 3
    }

grislyTotemSurvivor3 :: CardDef
grislyTotemSurvivor3 =
  (asset "05195" "Grisly Totem" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = 3
    }

theCouncilsCoffer2 :: CardDef
theCouncilsCoffer2 =
  (asset "05196" ("The Council's Coffer" <:> "What's in the Box?") 0 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 2
    , cdUses = Uses Lock (PerPlayer 1)
    , cdUnique = True
    }

augustLindquist :: CardDef
augustLindquist =
  (storyAsset "05227" ("August Lindquist" <:> "Elegant and Elusive") 0 ForTheGreaterGood)
    { cdCardTraits = setFromList [Cultist, SilverTwilight]
    , cdUnique = True
    , cdCost = Nothing
    }

puzzleBox :: CardDef
puzzleBox =
  (storyAsset "05228" ("Puzzle Box" <:> "Mysterious Device") 0 ForTheGreaterGood)
    { cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    }

esotericAtlas1 :: CardDef
esotericAtlas1 =
  (asset "05232" "Esoteric Atlas" 3 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    , cdLevel = 1
    }

investments :: CardDef
investments =
  (asset "05233" "Investments" 1 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Connection
    , cdUses = UsesWithLimit Supply (Static 0) (Static 10)
    }

deVermisMysteriis2 :: CardDef
deVermisMysteriis2 =
  (asset "05235" ("De Vermis Mysteriis" <:> "Signs of the Black Stars") 2 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdLevel = 2
    }

guidingSpirit1 :: CardDef
guidingSpirit1 =
  (asset "05236" "Guiding Spirit" 1 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Geist]
    , cdSlots = [#ally]
    , cdLevel = 1
    }

gavriellaMizrah :: CardDef
gavriellaMizrah =
  (storyAsset "05258" ("Gavriella Mizrah" <:> "Not Going Down That Easily") 2 ForTheGreaterGood)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Ally, Veteran]
    , cdUnique = True
    }

jeromeDavids :: CardDef
jeromeDavids =
  (storyAsset "05259" ("Jerome Davids" <:> "In Way Over His Head") 2 ForTheGreaterGood)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = setFromList [Ally, Assistant]
    , cdUnique = True
    }

pennyWhite :: CardDef
pennyWhite =
  (storyAsset "05260" ("Penny White" <:> "The Nightmare is Over") 2 ForTheGreaterGood)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Ally, Assistant]
    , cdUnique = True
    }

valentinoRivas :: CardDef
valentinoRivas =
  (storyAsset "05261" ("Valentino Rivas" <:> "Took You Long Enough") 2 ForTheGreaterGood)
    { cdSkills = [#agility, #wild]
    , cdCardTraits = setFromList [Ally, Socialite]
    , cdUnique = True
    }

mk1Grenades4 :: CardDef
mk1Grenades4 =
  (asset "05273" "Mk 1 Grenades" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Ranged]
    , cdUses = uses Supply 3
    , cdLevel = 4
    }

agencyBackup5 :: CardDef
agencyBackup5 =
  (asset "05274" "Agency Backup" 7 Guardian)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = setFromList [Ally, Agency]
    , cdLevel = 5
    , cdSlots = [#ally]
    }

studious3 :: CardDef
studious3 =
  permanent
    $ (asset "05276" "Studious" 0 Seeker)
      { cdCardTraits = singleton Talent
      , cdLevel = 3
      }

anotherDayAnotherDollar3 :: CardDef
anotherDayAnotherDollar3 =
  permanent
    $ (asset "05278" "Another Day, Another Dollar" 0 Rogue)
      { cdCardTraits = singleton Talent
      , cdLevel = 3
      }

dayanaEsperence3 :: CardDef
dayanaEsperence3 =
  (asset "05279" ("Dayana Esperence" <:> "Deals with \"Devils\"") 4 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Ally, Witch]
    , cdLevel = 3
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Secret 3
    }

annaKaslow4 :: CardDef
annaKaslow4 =
  (asset "05283" ("Anna Kaslow" <:> "Mysterious Soothsayer") 3 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Ally, Clairvoyant]
    , cdLevel = 4
    , cdSlots = [#ally]
    , cdCardInHandEffects = True
    , cdUnique = True
    }

hallowedMirror :: CardDef
hallowedMirror =
  (asset "05313" "Hallowed Mirror" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic, Occult, Blessed]
    , cdSlots = [#accessory]
    , cdBondedWith = [(3, "05314")]
    }

occultLexicon :: CardDef
occultLexicon =
  (asset "05316" "Occult Lexicon" 2 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSlots = [#hand]
    , cdBondedWith = [(3, "05317")]
    }

doubleDouble4 :: CardDef
doubleDouble4 =
  (asset "05320" "Double, Double" 4 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Ritual
    , cdExceptional = True
    , cdSlots = [#arcane]
    , cdLevel = 4
    }

wither4 :: CardDef
wither4 =
  (asset "05321" "Wither" 2 Mystic)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdLevel = 4
    }

sixthSense4 :: CardDef
sixthSense4 =
  (asset "05322" "Sixth Sense" 3 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdLevel = 4
    }

becky :: CardDef
becky =
  (asset "06006" ("Becky" <:> "Custom Marlin Model 1894") 2 Neutral)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#combat, #agility, #wild]
    , cdDeckRestrictions = [Signature "06001"]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 2
    , cdUnique = True
    }

bountyContracts :: CardDef
bountyContracts =
  permanent
    $ (asset "06010" "Bounty Contracts" 0 Neutral)
      { cdCardTraits = setFromList [Job]
      , cdDeckRestrictions = [Signature "06003"]
      , cdUses = uses Bounty 6
      }

tonys38LongColt :: CardDef
tonys38LongColt =
  (asset "06011" "Tony's .38 Long Colt" 3 Neutral)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#combat, #intellect, #wild]
    , cdDeckRestrictions = [Signature "06003"]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    }

gateBox :: CardDef
gateBox =
  (asset "06013" ("Gate Box" <:> "Worlds within Worlds") 3 Neutral)
    { cdCardTraits = setFromList [Item, Relic]
    , cdDeckRestrictions = [Signature "06004"]
    , cdUses = uses Charge 3
    , cdUnique = True
    }

patricesViolin :: CardDef
patricesViolin =
  (asset "06016" ("Patrice's Violin" <:> "My Muse") 2 Neutral)
    { cdCardTraits = setFromList [Item, Instrument]
    , cdDeckRestrictions = [Signature "06005"]
    , cdUnique = True
    , cdSlots = [#hand]
    , cdSkills = [#willpower, #agility, #wild]
    }

theHungeringBlade1 :: CardDef
theHungeringBlade1 =
  (asset "06018" ("The Hungering Blade" <:> "Calamitous Blade of Celephaïs") 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee, Relic, Cursed]
    , cdSlots = [#hand]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdAdditionalCost = Just (ShuffleBondedCost 3 "06019")
    , cdLevel = 1
    , cdBondedWith = [(3, "06019")]
    , cdUnique = True
    }

solemnVow :: CardDef
solemnVow =
  fast
    $ (asset "06020" "Solemn Vow" 0 Guardian)
      { cdSkills = [#willpower, #willpower]
      , cdCardTraits = singleton Spirit
      , cdKeywords = singleton Keyword.Myriad
      , cdCriteria =
          Just $ Criteria.InvestigatorExists $ affectsOthers $ NotYou <> InvestigatorAt YourLocation
      }

segmentOfOnyx1 :: CardDef
segmentOfOnyx1 =
  fast
    $ (asset "06021" "Segment of Onyx" 1 Seeker)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Item, Relic, Occult]
      , cdLevel = 1
      , cdBondedWith = [(1, "06022")]
      , cdKeywords = singleton Keyword.Myriad
      }

pendantOfTheQueen :: CardDef
pendantOfTheQueen =
  (asset "06022" ("Pendant of the Queen" <:> "Of Nothing at All") 0 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdKeywords = singleton (Keyword.Bonded 1 "06021")
    , cdUses = uses Charge 3
    , cdSlots = [#accessory]
    , cdCost = Nothing
    , cdUnique = True
    }

crystallizerOfDreams :: CardDef
crystallizerOfDreams =
  (asset "06024" "Crystallizer of Dreams" 1 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdBondedWith = [(1, "06025")]
    , cdAdditionalCost = Just (ShuffleBondedCost 1 "06025")
    , cdSlots = [#accessory]
    }

missDoyle1 :: CardDef
missDoyle1 =
  (asset "06030" ("Miss Doyle" <:> "Cat General of Ulthar") 3 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
    , cdLevel = 1
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdBondedWith = [(1, "06031"), (1, "06032"), (1, "06033")]
    , cdSlots = [#ally]
    , cdUnique = True
    }

hope :: CardDef
hope =
  fast
    $ (asset "06031" "Hope" 1 Survivor)
      { cdSkills = [#intellect, #combat]
      , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
      , cdKeywords = singleton (Keyword.Bonded 1 "06030")
      , cdUnique = True
      }

zeal :: CardDef
zeal =
  fast
    $ (asset "06032" "Zeal" 1 Survivor)
      { cdSkills = [#intellect, #agility]
      , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
      , cdKeywords = singleton (Keyword.Bonded 1 "06030")
      , cdUnique = True
      }

augur :: CardDef
augur =
  fast
    $ (asset "06033" "Augur" 1 Survivor)
      { cdSkills = [#combat, #agility]
      , cdCardTraits = setFromList [Ally, Creature, Dreamlands]
      , cdKeywords = singleton (Keyword.Bonded 1 "06030")
      , cdUnique = True
      }

kleptomania :: CardDef
kleptomania =
  (basicWeakness "06036" "Kleptomania")
    { cdCardTraits = setFromList [Madness, Talent]
    , cdDeckRestrictions = [MultiplayerOnly]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    }

randolphCarterExpertDreamer :: CardDef
randolphCarterExpertDreamer =
  (storyAsset "06059" ("Randolph Carter" <:> "Expert Dreamer") 3 BeyondTheGatesOfSleep)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

randolphCarterChainedToTheWakingWorld :: CardDef
randolphCarterChainedToTheWakingWorld =
  (storyAsset "06079" ("Randolph Carter" <:> "Chained to the Waking World") 3 WakingNightmare)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

drShivaniMaheswaran :: CardDef
drShivaniMaheswaran =
  (storyAsset "06080" ("Dr. Shivani Maheswaran" <:> "Emergency Physician") 0 WakingNightmare)
    { cdCardTraits = setFromList [Ally, Medic]
    , cdUnique = True
    , cdCost = Nothing
    }

dreamDiary :: CardDef
dreamDiary =
  (asset "06112" ("Dream Diary" <:> "Untranslated") 2 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    }

scrollOfProphecies :: CardDef
scrollOfProphecies =
  (asset "06116" "Scroll of Prophecies" 3 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 4
    , cdSlots = [#hand]
    }

jessicaHyde1 :: CardDef
jessicaHyde1 =
  (asset "06118" ("Jessica Hyde" <:> "Wrong Place, Wrong Time") 3 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Wayfarer, Cursed]
    , cdLevel = 1
    , cdSlots = [#ally]
    , cdUnique = True
    }

virgilGray :: CardDef
virgilGray =
  (storyAsset "06144" ("Virgil Gray" <:> "Writer of Strange Tales") 0 TheSearchForKadath)
    { cdCardTraits = setFromList [Ally, Dreamer]
    , cdUnique = True
    }

tetsuoMori :: CardDef
tetsuoMori =
  (asset "06155" ("Tetsuo Mori" <:> "Too Noble for His Own Good") 3 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Police]
    , cdSlots = [#ally]
    , cdUnique = True
    }

otherworldCodex2 :: CardDef
otherworldCodex2 =
  (asset "06158" "Otherworld Codex" 3 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    , cdLevel = 2
    }

dreamEnhancingSerum :: CardDef
dreamEnhancingSerum =
  (asset "06159" "Dream-Enhancing Serum" 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Science]
    , cdSlots = [#arcane]
    }

gregoryGry :: CardDef
gregoryGry =
  (asset "06162" ("Gregory Gry" <:> "Muckraker") 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Criminal, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Resource 9
    }

healingWords :: CardDef
healingWords =
  (asset "06163" "Healing Words" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

versatile2 :: CardDef
versatile2 =
  permanent
    $ (asset "06167" "Versatile" 0 Neutral)
      { cdCardTraits = singleton Talent
      , cdLevel = 2
      }

theSilverKey :: CardDef
theSilverKey =
  (storyAsset "06189" ("The Silver Key" <:> "Key to the Gate of Dreams") 2 AThousandShapesOfHorror)
    { cdSkills = [#willpower, #wild, #wild]
    , cdCardTraits = setFromList [Item, Charm, Relic]
    , cdUnique = True
    , cdSlots = [#accessory]
    }

thirtyFiveWinchester :: CardDef
thirtyFiveWinchester =
  (asset "06195" ".35 Winchester" 4 Guardian)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 5
    }

safeguard2 :: CardDef
safeguard2 =
  (asset "06196" "Safeguard" 2 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = 2
    }

burglary2 :: CardDef
burglary2 =
  (asset "06200" "Burglary" 1 Rogue)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Talent, Illicit]
    , cdLevel = 2
    }

moonstone :: CardDef
moonstone =
  (asset "06203" "Moonstone" 3 Survivor)
    { cdCardTraits = setFromList [Item, Relic, Dreamlands]
    , cdSlots = [#accessory]
    , cdCriteria = Just Criteria.Never
    , cdCommitRestrictions = [SelfCanCommitWhen NoOne]
    , cdCardInHandEffects = True
    , cdCardInDiscardEffects = True
    }

virgilGrayTrulyInspired :: CardDef
virgilGrayTrulyInspired =
  (storyAsset "06224" ("Virgil Gray" <:> "Truly Inspired") 0 DarkSideOfTheMoon)
    { cdCardTraits = setFromList [Ally, Dreamer]
    , cdUnique = True
    , cdCost = Nothing
    }

theCaptain :: CardDef
theCaptain =
  (storyAsset "06225" ("The Captain" <:> "Dreamlands Navigator") 0 DarkSideOfTheMoon)
    { cdCardTraits = setFromList [Ally, Dreamer]
    , cdUnique = True
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

dreamDiaryDreamsOfAnExplorer3 :: CardDef
dreamDiaryDreamsOfAnExplorer3 =
  (asset "06236" ("Dream Diary" <:> "Dreams of an Explorer") 2 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    , cdLevel = 3
    }

dreamDiaryDreamsOfAMadman3 :: CardDef
dreamDiaryDreamsOfAMadman3 =
  (asset "06237" ("Dream Diary" <:> "Dreams of a Madman") 2 Seeker)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    , cdLevel = 3
    }

dreamDiaryDreamsOfAChild3 :: CardDef
dreamDiaryDreamsOfAChild3 =
  (asset "06238" ("Dream Diary" <:> "Dreams of a Child") 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome, Charm]
    , cdSlots = [#hand]
    , cdBondedWith = [(1, "06113")]
    , cdLevel = 3
    }

haste2 :: CardDef
haste2 =
  (asset "06239" "Haste" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdLevel = 2
    , cdLimits = [LimitPerInvestigator 1]
    }

empowerSelfStamina2 :: CardDef
empowerSelfStamina2 =
  (asset "06241" ("Empower Self" <:> "Stamina") 3 Mystic)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = 2
    , cdKeywords = singleton Keyword.Myriad
    }

empowerSelfAlacrity2 :: CardDef
empowerSelfAlacrity2 =
  (asset "06242" ("Empower Self" <:> "Alacrity") 3 Mystic)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = 2
    , cdKeywords = singleton Keyword.Myriad
    }

empowerSelfAcuity2 :: CardDef
empowerSelfAcuity2 =
  (asset "06243" ("Empower Self" <:> "Acuity") 3 Mystic)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    , cdLevel = 2
    , cdKeywords = singleton Keyword.Myriad
    }

twilaKatherinePrice3 :: CardDef
twilaKatherinePrice3 =
  (asset "06244" ("Twila Katherine Price" <:> "Lost in a Dream") 3 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ally, Artist, Dreamer]
    , cdSlots = [#ally]
    , cdLevel = 3
    , cdUnique = True
    }

richardUptonPickman :: CardDef
richardUptonPickman =
  (storyAsset "06266" ("Richard Upton Pickman" <:> "Venerable Ghoul") 0 PointOfNoReturn)
    { cdCardTraits = setFromList [Ally, Ghoul, Artist]
    , cdUnique = True
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

emptyVessel4 :: CardDef
emptyVessel4 =
  (asset "06276" ("Empty Vessel" <:> "Abandoned by the Gods") 1 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = 4
    , cdUnique = True
    , cdUses = uses Charge 0
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdBondedWith = [(1, "06277")]
    }

wishEater :: CardDef
wishEater =
  (asset "06277" ("Wish Eater" <:> "Jewel of the Gods") 0 Guardian)
    { cdCardTraits = setFromList [Item, Relic, Blessed]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdKeywords = singleton (Keyword.Bonded 1 "06277")
    , cdCost = Nothing
    }

oldBookOfLore3 :: CardDef
oldBookOfLore3 =
  (asset "06279" "Old Book of Lore" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdAlternateCardCodes = ["01686"]
    , cdUses = uses Secret 2
    , cdLevel = 3
    }

garroteWire2 :: CardDef
garroteWire2 =
  (asset "06280" "Garrote Wire" 2 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon]
    , cdSlots = [#accessory]
    , cdLevel = 2
    }

delilahORourke3 :: CardDef
delilahORourke3 =
  (asset "06281" ("Delilah O'Rourke" <:> "Syndicate Assassin") 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Ally, Criminal, Syndicate]
    , cdSlots = [#ally]
    , cdLevel = 3
    , cdUnique = True
    }

summonedHound1 :: CardDef
summonedHound1 =
  (asset "06282" "Summoned Hound" 3 Mystic)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Ally, Summon]
    , cdSlots = [#ally, #arcane]
    , cdAdditionalCost = Just (ShuffleBondedCost 1 "06283")
    , cdLevel = 1
    , cdBondedWith = [(1, "06283")]
    }

theBlackCat5 :: CardDef
theBlackCat5 =
  (asset "06285" ("The Black Cat" <:> "A Liar, or a Prophet, or Both") 2 Neutral)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Ally, Avatar, Dreamlands]
    , cdSlots = [#ally]
    , cdLevel = 5
    , cdUnique = True
    }

spiritualResolve5 :: CardDef
spiritualResolve5 =
  (asset "06323" "Spiritual Resolve" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdKeywords = singleton Keyword.Myriad
    , cdSlots = [#arcane]
    , cdLevel = 5
    }

abigailForeman4 :: CardDef
abigailForeman4 =
  (asset "06324" ("Abigail Foreman" <:> "Library Intern") 3 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = 4
    }

joeyTheRatVigil3 :: CardDef
joeyTheRatVigil3 =
  (asset "06326" ("Joey \"The Rat\" Vigil" <:> "Lookin' Out for #1") 2 Rogue)
    { cdCardTraits = setFromList [Ally, Criminal]
    , cdSkills = [#intellect, #agility]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdLevel = 3
    }

sawedOffShotgun5 :: CardDef
sawedOffShotgun5 =
  (asset "06327" "Sawed-Off Shotgun" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdLevel = 5
    , cdUses = uses Ammo 2
    , cdSlots = [#hand]
    }

mindsEye2 :: CardDef
mindsEye2 =
  (asset "06328" "Mind's Eye" 3 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdUses = uses Secret 3
    , cdSlots = [#arcane, #arcane]
    , cdLevel = 2
    }

shiningTrapezohedron4 :: CardDef
shiningTrapezohedron4 =
  (asset "06329" "Shining Trapezohedron" 1 Mystic)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Item, Relic]
    , cdSlots = [#accessory]
    , cdLevel = 4
    , cdUnique = True
    }

nightmareBauble3 :: CardDef
nightmareBauble3 =
  (asset "06330" "Nightmare Bauble" 1 Survivor)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    , cdLevel = 3
    , cdBondedWith = [(3, "06331")]
    }

scavenging2 :: CardDef
scavenging2 =
  (asset "06332" "Scavenging" 1 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = 2
    }

guardianAngel :: CardDef
guardianAngel =
  (asset "07006" "Guardian Angel" 2 Neutral)
    { cdCardTraits = setFromList [Ritual, Blessed]
    , cdSkills = [#willpower, #combat, #wild]
    , cdDeckRestrictions = [Signature "07001"]
    }

showmanship :: CardDef
showmanship =
  (asset "07012" "Showmanship" 1 Neutral)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#combat, #agility, #wild]
    , cdDeckRestrictions = [Signature "07004"]
    }

occultScraps :: CardDef
occultScraps =
  (weakness "07013" "Occult Scraps")
    { cdCardTraits = setFromList [Item]
    , cdCriteria = Just Criteria.Never
    , cdCardInHandEffects = True
    , cdCost = Just (StaticCost 0)
    }

seaChangeHarpoon :: CardDef
seaChangeHarpoon =
  (asset "07014" "Sea Change Harpoon" 3 Neutral)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat, #wild]
    , cdDeckRestrictions = [Signature "07005"]
    , cdSlots = [#hand]
    , cdUnique = True
    }

silassNet :: CardDef
silassNet =
  (asset "07015" "Silas's Net" 2 Neutral)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#agility, #wild]
    , cdDeckRestrictions = [Signature "07005"]
    , cdSlots = [#hand]
    , cdUnique = True
    }

bookOfPsalms :: CardDef
bookOfPsalms =
  (asset "07017" "Book of Psalms" 3 Guardian)
    { cdCardTraits = setFromList [Item, Tome, Blessed]
    , cdSkills = [#willpower]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

blessedBlade :: CardDef
blessedBlade =
  (asset "07018" "Blessed Blade" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee, Blessed]
    , cdSkills = [#combat]
    , cdSlots = [#hand]
    }

riteOfSanctification :: CardDef
riteOfSanctification =
  (asset "07019" "Rite of Sanctification" 0 Guardian)
    { cdCardTraits = setFromList [Ritual, Blessed]
    , cdSkills = [#intellect]
    , cdSlots = [#arcane]
    , cdKeywords = singleton $ seal $ SealUpTo 5 #bless
    }

cryptographicCipher :: CardDef
cryptographicCipher =
  (asset "07021" "Cryptographic Cipher" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    }

swordCane :: CardDef
swordCane =
  (asset "07029" "Sword Cane" 2 Mystic)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

keenEye :: CardDef
keenEye =
  (asset "07152" "Keen Eye" 2 Guardian)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#intellect, #combat]
    }

abyssalTome2 :: CardDef
abyssalTome2 =
  (asset "07159" "Abyssal Tome" 2 Mystic)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdLevel = 2
    }

holyRosary2 :: CardDef
holyRosary2 =
  (asset "07220" "Holy Rosary" 2 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = 2
    }

hardKnocks4 :: CardDef
hardKnocks4 =
  (asset "07266" "Hard Knocks" 2 Rogue)
    { cdSkills = [#combat, #combat, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Resource 2
    , cdLevel = 4
    }

ancestralKnowledge3 :: CardDef
ancestralKnowledge3 =
  permanent
    $ (asset "07303" "Ancestral Knowledge" 0 Seeker)
      { cdCardTraits = singleton Talent
      , cdKeywords = setFromList [Keyword.Exceptional]
      , cdLevel = 3
      }

livreDeibon :: CardDef
livreDeibon =
  (asset "08005" ("Livre d'Eibon" <:> "Hyperborean Grimoire") 2 Neutral)
    { cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSkills = [#willpower, #willpower, #wild]
    , cdUnique = True
    , cdSlots = [#hand]
    , cdDeckRestrictions = [Signature "08004"]
    }

-- TODO: if we ever care about deck size need to encode that somehow
forcedLearning :: CardDef
forcedLearning =
  permanent
    $ (asset "08031" "Forced Learning" 0 Seeker)
      { cdCardTraits = setFromList [Talent, Ritual]
      , cdDeckRestrictions = [PerDeckLimit 1, PurchaseAtDeckCreation]
      }

theBlackFan3 :: CardDef
theBlackFan3 =
  (asset "08057" ("The Black Fan" <:> "Symbol of Power") 3 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #agility, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdExceptional = True
    , cdLevel = 3
    }

dragonPole :: CardDef
dragonPole =
  (asset "08060" "Dragon Pole" 3 Mystic)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand, #hand]
    }

schoffnersCatalogue :: CardDef
schoffnersCatalogue =
  (asset "08072" "Schoffner's Catalogue" 2 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 5
    }

bangleOfJinxes1 :: CardDef
bangleOfJinxes1 =
  (asset "08075" "Bangle of Jinxes" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSlots = [#accessory]
    , cdUses = uses Charge 1
    , cdLevel = 1
    }

medicalStudent :: CardDef
medicalStudent =
  (multiClassAsset "08083" "Medical Student" 2 [Guardian, Seeker])
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic, Science]
    , cdSlots = [#ally]
    }

michaelLeigh5 :: CardDef
michaelLeigh5 =
  (multiClassAsset "08086" ("Michael Leigh" <:> "Experienced Hunter") 4 [Guardian, Seeker])
    { cdSkills = [#intellect, #combat, #wild]
    , cdCardTraits = setFromList [Ally, Detective]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdUses = uses Evidence 0
    , cdLevel = 5
    }

brandOfCthugha1 :: CardDef
brandOfCthugha1 =
  (multiClassAsset "08090" "Brand Of Cthugha" 2 [Guardian, Mystic])
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = 1
    , cdUses = uses Charge 6
    , cdSlots = [#arcane]
    }

brandOfCthugha4 :: CardDef
brandOfCthugha4 =
  (multiClassAsset "08092" "Brand Of Cthugha" 2 [Guardian, Mystic])
    { cdSkills = [#combat, #willpower]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = 4
    , cdUses = uses Charge 9
    , cdSlots = [#arcane]
    }

geneBeauregard3 :: CardDef
geneBeauregard3 =
  (multiClassAsset "08099" ("Gené Beauregard" <:> "Intrepid Explorer") 5 [Seeker, Rogue])
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSkills = [#intellect, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = 3
    }

preciousMementoFromAFormerLife4 :: CardDef
preciousMementoFromAFormerLife4 =
  (multiClassAsset "08114" ("Precious Memento" <:> "From a Former Life") 3 [Rogue, Survivor])
    { cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdSkills = [#wild, #wild]
    , cdSlots = [#accessory]
    , cdLevel = 4
    }

preciousMementoFromAFutureLife4 :: CardDef
preciousMementoFromAFutureLife4 =
  (multiClassAsset "08115" ("Precious Memento" <:> "From a Future Life") 3 [Rogue, Survivor])
    { cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdSkills = [#wild, #wild]
    , cdSlots = [#accessory]
    , cdLevel = 4
    }

inTheThickOfIt :: CardDef
inTheThickOfIt =
  permanent
    $ (asset "08125" "In the Thick of it" 0 Neutral)
      { cdCardTraits = singleton Curse
      , cdPurchaseTrauma = PurchaseAnyTrauma 2
      }

runicAxe :: CardDef
runicAxe =
  (asset "09022" "Runix Axe" 4 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#combat]
    , cdSlots = [#hand, #hand]
    }

guardDog2 :: CardDef
guardDog2 =
  (asset "09034" "Guard Dog" 3 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    , cdLevel = 2
    }

handcuffs2 :: CardDef
handcuffs2 =
  fast
    $ (asset "09035" "Handcuffs" 1 Guardian)
      { cdCardTraits = setFromList [Item, Police]
      , cdSkills = [#combat, #agility]
      , cdLevel = 2
      }

empiricalHypothesis :: CardDef
empiricalHypothesis =
  (asset "09041" "Empirical Hypothesis" 2 Seeker)
    { cdCardTraits = setFromList [Talent, Science]
    , cdSkills = [#intellect]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdLimits = [LimitPerInvestigator 1]
    , cdUses = uses Evidence 0
    , cdCustomizations =
        mapFromList
          [ (PessimisticOutlook, 1)
          , (TrialAndError, 1)
          , (IndepedentVariable, 1)
          , (FieldResearch, 1)
          , (PeerReview, 2)
          , (ResearchGrant, 2)
          , (IrrefutableProof, 3)
          , (AlternativeHypothesis, 4)
          ]
    }

fingerprintKit4 :: CardDef
fingerprintKit4 =
  (asset "09057" "Fingerprint Kit" 5 Seeker)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [#hand]
    , cdUses = uses Supply 3
    , cdLevel = 4
    }

chuckFergus2 :: CardDef
chuckFergus2 =
  (asset "09072" ("Chuck Fergus" <:> "O'Bannion Driver") 4 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdSlots = [#ally]
    , cdLevel = 2
    , cdUnique = True
    }

physicalTraining2 :: CardDef
physicalTraining2 =
  (asset "50001" "Physical Training" 0 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = 2
    }

hyperawareness2 :: CardDef
hyperawareness2 =
  (asset "50003" "Hyperawareness" 0 Seeker)
    { cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = 2
    }

hardKnocks2 :: CardDef
hardKnocks2 =
  (asset "50005" "Hard Knocks" 0 Rogue)
    { cdSkills = [#combat, #combat, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = 2
    }

arcaneStudies2 :: CardDef
arcaneStudies2 =
  (asset "50007" "Arcane Studies" 0 Mystic)
    { cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = 2
    }

digDeep2 :: CardDef
digDeep2 =
  (asset "50009" "Dig Deep" 0 Survivor)
    { cdSkills = [#willpower, #willpower, #agility, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = 2
    }

rabbitsFoot3 :: CardDef
rabbitsFoot3 =
  (asset "50010" "Rabbit's Foot" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Charm]
    , cdLevel = 3
    , cdSlots = [#accessory]
    }

bandolier2 :: CardDef
bandolier2 =
  (asset "51001" "Bandolier" 2 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item]
    , cdSlots = [#body]
    , cdLevel = 2
    }

blackjack2 :: CardDef
blackjack2 =
  (asset "51002" "Blackjack" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = 2
    }

strangeSolutionEmpoweringElixir4 :: CardDef
strangeSolutionEmpoweringElixir4 =
  (asset "51004" ("Strange Solution" <:> "Empowering Elixir") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#intellect, #intellect]
    , cdLevel = 4
    , cdUses = uses Supply 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveIdentifiedTheSolution
    }

riteOfSeeking2 :: CardDef
riteOfSeeking2 =
  (asset "51007" "Rite of Seeking" 4 Mystic)
    { cdCardTraits = singleton Spell
    , cdSkills = [#intellect]
    , cdLevel = 2
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdAlternateCardCodes = ["01689"]
    }

clarityOfMind3 :: CardDef
clarityOfMind3 =
  (asset "51008" "Clarity of Mind" 2 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    , cdLevel = 3
    }

thirtyTwoColt2 :: CardDef
thirtyTwoColt2 =
  (asset "52001" ".32 Colt" 2 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 6
    , cdSlots = [#hand]
    , cdLevel = 2
    }

archaicGlyphsMarkingsOfIsis3 :: CardDef
archaicGlyphsMarkingsOfIsis3 =
  (asset "52004" ("Archaic Glyphs" <:> "Markings of Isis") 2 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = 3
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheGlyphs
    }

stealth3 :: CardDef
stealth3 =
  (asset "52005" "Stealth" 2 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = 3
    }

suggestion1 :: CardDef
suggestion1 =
  (asset "52006" "Suggestion" 3 Rogue)
    { cdCardTraits = singleton Spell
    , cdSkills = [#willpower]
    , cdUses = uses Charge 3
    , cdSlots = [#arcane]
    , cdLevel = 1
    }

alchemicalTransmutation2 :: CardDef
alchemicalTransmutation2 =
  (asset "52007" "Alchemical Transmutation" 0 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdUses = uses Charge 4
    , cdSlots = [#arcane]
    , cdLevel = 2
    }

lantern2 :: CardDef
lantern2 =
  (asset "52009" "Lantern" 1 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    , cdLevel = 2
    }

gravediggersShovel2 :: CardDef
gravediggersShovel2 =
  (asset "52010" "Gravedigger's Shovel" 1 Survivor)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = 2
    }

survivalKnife2 :: CardDef
survivalKnife2 =
  (asset "53002" "Survival Knife" 2 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = 2
    }

ancientStoneTransientThoughts4 :: CardDef
ancientStoneTransientThoughts4 =
  (asset "53004" ("Ancient Stone" <:> "Transient Thoughts") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#agility, #agility]
    , cdSlots = [#hand]
    , cdUses = uses Secret 0
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = 4
    }

decoratedSkull3 :: CardDef
decoratedSkull3 =
  (asset "53005" ("Decorated Skull" <:> "Doom Begets Doom") 0 Rogue)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#accessory]
    , cdUses = uses Charge 0
    , cdLevel = 3
    }

coltVestPocket2 :: CardDef
coltVestPocket2 =
  (asset "53006" "Colt Vest Pocket" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 5
    , cdSlots = [#hand]
    , cdLevel = 2
    }

mistsOfRlyeh2 :: CardDef
mistsOfRlyeh2 =
  (asset "53007" "Mists of R'lyeh" 2 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 5
    , cdLevel = 2
    }

theChthonianStone3 :: CardDef
theChthonianStone3 =
  (asset "53008" ("The Chthonian Stone" <:> "Stygian Waymark") 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdLevel = 3
    , cdUses = uses Charge 3
    , cdKeywords =
        singleton
          $ seal
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Token.Skull, Token.Cultist, Token.Tablet, Token.ElderThing]
    }

onYourOwn3_Exceptional :: CardDef
onYourOwn3_Exceptional =
  permanent
    $ (asset "53010" "On Your Own" 0 Survivor)
      { cdCardTraits = singleton Talent
      , cdLimits = [LimitPerInvestigator 1]
      , cdExceptional = True
      , cdLevel = 3
      }

backpack2 :: CardDef
backpack2 =
  (asset "53011" "Backpack" 1 Neutral)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Item
    , cdSlots = [#body]
    , cdLevel = 2
    }

dendromorphosis :: CardDef
dendromorphosis =
  (basicWeakness "53012" ("Dendromorphosis" <:> "\"Natural\" Transformation"))
    { cdSlots = [#hand, #hand]
    , cdCardTraits = setFromList [Curse, Flora]
    , cdCost = Nothing
    }

theStarXvii3 :: CardDef
theStarXvii3 =
  (asset "54001" ("The Star • XVII" <:> "You Have Been Chosen") 3 Guardian)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 3
    , cdCardInHandEffects = True
    }

hallowedMirror3 :: CardDef
hallowedMirror3 =
  (asset "54002" "Hallowed Mirror" 2 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Item, Relic, Occult, Blessed]
    , cdSlots = [#accessory]
    , cdLevel = 3
    }

theWorldXxi3 :: CardDef
theWorldXxi3 =
  (asset "54003" ("The World • XXI" <:> "The Journey is Complete") 3 Seeker)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 3
    , cdCardInHandEffects = True
    }

occultLexicon3 :: CardDef
occultLexicon3 =
  (asset "54004" "Occult Lexicon" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSlots = [#hand]
    , cdLevel = 3
    }

knightOfSwords3 :: CardDef
knightOfSwords3 =
  (asset "54005" ("Knight of Swords" <:> "Charge Ever Onward") 3 Rogue)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 3
    , cdCardInHandEffects = True
    }

wellConnected3 :: CardDef
wellConnected3 =
  (asset "54006" "Well Connected" 2 Rogue)
    { cdCardTraits = singleton Condition
    , cdSkills = [#intellect, #agility]
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = 3
    }

theHierophantV3 :: CardDef
theHierophantV3 =
  (asset "54007" ("The Hierophant • V" <:> "Your True Master Awaits") 3 Mystic)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 3
    , cdCardInHandEffects = True
    }

signMagick3 :: CardDef
signMagick3 =
  fast
    (asset "54008" "Sign Magick" 3 Mystic)
      { cdSkills = [#willpower, #intellect]
      , cdCardTraits = setFromList [Ritual, Talent]
      , cdSlots = [#hand]
      , cdLevel = 3
      }

nineOfRods3 :: CardDef
nineOfRods3 =
  (asset "54009" ("Nine of Rods" <:> "Every Trial a Lesson") 3 Survivor)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 3
    , cdCardInHandEffects = True
    }

theFool03 :: CardDef
theFool03 =
  (asset "54011" ("The Fool • 0" <:> "Unlimited Potential") 3 Neutral)
    { cdCardTraits = singleton Tarot
    , cdSlots = [#tarot]
    , cdLevel = 3
    , cdCardInHandEffects = True
    }

moonPendant2 :: CardDef
moonPendant2 =
  (asset "54012" "Moon Pendant" 2 Neutral)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdLevel = 2
    , cdCardInHandEffects = True
    }

observed4 :: CardDef
observed4 =
  permanent
    $ (asset "54013" "Observed" 0 Neutral)
      { cdCardTraits = singleton Blessed
      , cdLevel = 4
      , cdLimits = [LimitPerInvestigator 1]
      }

theDevilXv :: CardDef
theDevilXv =
  (basicWeakness "54015" ("The Devil • XV" <:> "Your Shadow Hungers"))
    { cdCardTraits = setFromList [Omen, Tarot]
    , cdSlots = [#tarot]
    , cdCardInHandEffects = True
    , cdCanReplace = False
    , cdRevelation = NoRevelation
    , cdCost = Just (StaticCost 3)
    }

randallCho :: CardDef
randallCho =
  (asset "60102" ("Randall Cho" <:> "Concerned Brother") 2 Guardian)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Medic]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdDeckRestrictions = [Signature "60101"]
    }

boxingGloves :: CardDef
boxingGloves =
  (asset "60105" "Boxing Gloves" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon]
    , cdSlots = [#hand, #hand]
    }

fleshWard :: CardDef
fleshWard =
  (asset "60106" "Flesh Ward" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

greteWagner :: CardDef
greteWagner =
  (asset "60107" ("Grete Wagner" <:> "The Purifier") 5 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Ally, Hunter]
    , cdSlots = [#ally]
    , cdUnique = True
    }

relentless :: CardDef
relentless =
  (asset "60109" "Relentless" 0 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Talent
    }

safeguard :: CardDef
safeguard =
  (asset "60110" "Safeguard" 2 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Talent
    }

boxingGloves3 :: CardDef
boxingGloves3 =
  (asset "60127" "Boxing Gloves" 2 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon]
    , cdSlots = [#hand, #hand]
    , cdLevel = 3
    }

greteWagner3 :: CardDef
greteWagner3 =
  (asset "60128" ("Grete Wagner" <:> "The Purifier") 5 Guardian)
    { cdSkills = [#combat, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Hunter]
    , cdSlots = [#ally]
    , cdLevel = 3
    , cdUnique = True
    }

physicalTraining4 :: CardDef
physicalTraining4 =
  (asset "60131" "Physical Training" 2 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Talent]
    , cdUses = uses Resource 2
    , cdLevel = 4
    }

vaultOfKnowledge :: CardDef
vaultOfKnowledge =
  (asset "60202" "Vault of Knowledge" 3 Seeker)
    { cdSkills = [#willpower, #agility, #wild]
    , cdCardTraits = singleton Talent
    , cdDeckRestrictions = [Signature "60202"]
    }

arcaneEnlightenment :: CardDef
arcaneEnlightenment =
  (asset "60205" "Arcane Enlightenment" 2 Seeker)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Ritual]
    , cdSlots = [#arcane]
    }

celaenoFragments :: CardDef
celaenoFragments =
  (asset "60206" ("Celaeno Fragments" <:> "Book of Books") 1 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUnique = True
    , cdSlots = [#hand]
    }

discOfItzamna :: CardDef
discOfItzamna =
  (asset "60207" ("Disc of Itzamna" <:> "Protective Amulet") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [#accessory]
    }

encyclopedia :: CardDef
encyclopedia =
  (asset "60208" "Encyclopedia" 2 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 5
    , cdSlots = [#hand]
    }

feedTheMind :: CardDef
feedTheMind =
  (asset "60209" "Feed the Mind" 3 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdUses = uses Secret 3
    , cdSlots = [#arcane]
    }

forbiddenTome :: CardDef
forbiddenTome =
  (asset "60210" ("Forbidden Tome" <:> "Untranslated") 1 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdUses = uses Secret 5
    , cdSlots = [#hand]
    }

higherEducation :: CardDef
higherEducation =
  (asset "60211" "Higher Education" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent]
    }

whittonGreene :: CardDef
whittonGreene =
  (asset "60213" ("Whitton Greene" <:> "Hunter of Rare Books") 4 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    }

libraryDocent1 :: CardDef
libraryDocent1 =
  (asset "60220" "Library Docent" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSlots = [#ally]
    , cdLevel = 1
    }

esotericAtlas2 :: CardDef
esotericAtlas2 =
  (asset "60222" "Esoteric Atlas" 3 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdLevel = 2
    , cdUses = uses Secret 4
    }

whittonGreene2 :: CardDef
whittonGreene2 =
  (asset "60223" ("Whitton Greene" <:> "Hunter of Rare Books") 4 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdLevel = 2
    }

forbiddenTomeDarkKnowledge3 :: CardDef
forbiddenTomeDarkKnowledge3 =
  (asset "60229" ("Forbidden Tome" <:> "Dark Knowledge") 1 Seeker)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSlots = [#hand]
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheTome
    , cdLevel = 3
    }

forbiddenTomeSecretsRevealed3 :: CardDef
forbiddenTomeSecretsRevealed3 =
  (asset "60230" ("Forbidden Tome" <:> "Secrets Revealed") 1 Seeker)
    { cdSkills = [#intellect, #agility, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSlots = [#hand]
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheTome
    , cdLevel = 3
    }

farsight4 :: CardDef
farsight4 =
  (asset "60231" "Farsight" 2 Seeker)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = singleton Ritual
    , cdSlots = [#arcane]
    , cdLevel = 4
    }

miskatonicArchaeologyFunding4 :: CardDef
miskatonicArchaeologyFunding4 =
  permanent
    $ (asset "60232" "Miskatonic Archaeology Funding" 0 Seeker)
      { cdCardTraits = singleton Grant
      , cdLevel = 4
      }

theNecronomiconPetrusDeDaciaTranslation5 :: CardDef
theNecronomiconPetrusDeDaciaTranslation5 =
  (asset "60233" ("The Necronomicon" <:> "Petrus de Dacia Translation") 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 6
    , cdLevel = 5
    , cdSkills = [#intellect, #intellect, #intellect, #intellect, #intellect]
    }

lockpicks :: CardDef
lockpicks =
  (asset "60305" "Lockpicks" 3 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool, Illicit]
    , cdSlots = [#hand]
    }

mauserC96 :: CardDef
mauserC96 =
  (asset "60306" "Mauser C96" 4 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 5
    }

lonnieRitter :: CardDef
lonnieRitter =
  (asset "60309" ("Lonnie Ritter" <:> "Feisty Mechanic") 4 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    }

leatherJacket :: CardDef
leatherJacket =
  fast
    $ (asset "60310" "Leather Jacket" 2 Rogue)
      { cdSkills = [#combat]
      , cdCardTraits = setFromList [Item, Armor]
      , cdSlots = [#body]
      }

streetwise :: CardDef
streetwise =
  (asset "60311" "Streetwise" 0 Rogue)
    { cdCardTraits = singleton Talent
    , cdSkills = [#intellect, #agility]
    }

liquidCourage1 :: CardDef
liquidCourage1 =
  (asset "60320" "Liquid Courage" 1 Rogue)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Item, Illicit]
    , cdUses = uses Supply 4
    , cdLevel = 1
    }

mauserC962 :: CardDef
mauserC962 =
  (asset "60321" "Mauser C96" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 5
    , cdLevel = 2
    }

luckyCigaretteCase3 :: CardDef
luckyCigaretteCase3 =
  (asset "60326" "Lucky Cigarette Case" 2 Rogue)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdLevel = 3
    }

sharpshooter3 :: CardDef
sharpshooter3 =
  (asset "60327" "Sharpshooter" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Talent
    , cdLevel = 3
    }

berettaM19184 :: CardDef
berettaM19184 =
  (asset "60331" "Beretta M1918" 4 Rogue)
    { cdSkills = [#combat, #combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 4
    , cdSlots = [#hand, #hand]
    , cdLevel = 4
    }

chuckFergus5 :: CardDef
chuckFergus5 =
  (asset "60332" ("Chuck Fergus" <:> "O'Bannion Driver") 3 Rogue)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Criminal]
    , cdSlots = [#ally]
    , cdLevel = 5
    , cdUnique = True
    }

arbiterOfFates :: CardDef
arbiterOfFates =
  (asset "60402" "Arbiter of Fates" 3 Mystic)
    { cdSkills = [#willpower, #agility, #wild]
    , cdCardTraits = singleton Talent
    , cdDeckRestrictions = [Signature "60401"]
    }

scryingMirror :: CardDef
scryingMirror =
  (asset "60406" "Scrying Mirror" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

azureFlame :: CardDef
azureFlame =
  (asset "60407" "Azure Flame" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

clairvoyance :: CardDef
clairvoyance =
  (asset "60408" "Clairvoyance" 4 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

ineffableTruth :: CardDef
ineffableTruth =
  (asset "60409" "Ineffable Truth" 3 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

familiarSpirit :: CardDef
familiarSpirit =
  (asset "60410" "Familiar Spirit" 1 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Creature, Summon]
    , cdSlots = [#ally]
    }

crystalPendulum :: CardDef
crystalPendulum =
  (asset "60411" "Crystal Pendulum" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    }

robesOfEndlessNight :: CardDef
robesOfEndlessNight =
  (asset "60412" "Robes of Endless Night" 3 Mystic)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    }

grotesqueStatue2 :: CardDef
grotesqueStatue2 =
  (asset "60421" "Grotesque Statue" 3 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 2
    , cdUses = uses Charge 3
    , cdSlots = [#hand]
    }

robesOfEndlessNight2 :: CardDef
robesOfEndlessNight2 =
  (asset "60422" "Robes of Endless Night" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    , cdLevel = 2
    }

azureFlame3 :: CardDef
azureFlame3 =
  (asset "60425" "Azure Flame" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = 3
    }

clairvoyance3 :: CardDef
clairvoyance3 =
  (asset "60426" "Clairvoyance" 4 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = 3
    }

ineffableTruth3 :: CardDef
ineffableTruth3 =
  (asset "60427" "Ineffable Truth" 3 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = 3
    }

arcaneStudies4 :: CardDef
arcaneStudies4 =
  (asset "60428" "Arcane Studies" 2 Mystic)
    { cdSkills = [#willpower, #willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Talent]
    , cdLevel = 4
    , cdUses = uses Resource 2
    }

azureFlame5 :: CardDef
azureFlame5 =
  (asset "60430" "Azure Flame" 3 Mystic)
    { cdSkills = [#willpower, #combat, #combat]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = 5
    }

clairvoyance5 :: CardDef
clairvoyance5 =
  (asset "60431" "Clairvoyance" 4 Mystic)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = 5
    }

ineffableTruth5 :: CardDef
ineffableTruth5 =
  (asset "60432" "Ineffable Truth" 3 Mystic)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    , cdLevel = 5
    }

eighteenDerringer :: CardDef
eighteenDerringer =
  (asset "60505" ".18 Derringer" 3 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 2
    , cdSlots = [#hand]
    }

grimmsFairyTales :: CardDef
grimmsFairyTales =
  (asset "60506" "Grimm's Fairy Tales" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUses = uses Secret 4
    , cdSlots = [#hand]
    }

oldKeyring :: CardDef
oldKeyring =
  (asset "60507" "Old Keyring" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdUses = uses Uses.Key 2
    , cdSlots = [#hand]
    }

grannyOrne :: CardDef
grannyOrne =
  (asset "60508" ("Granny Orne" <:> "Tough Old Bird") 4 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    }

mysteriousRaven :: CardDef
mysteriousRaven =
  (asset "60509" "Mysterious Raven" 1 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Creature]
    , cdSlots = [#ally]
    }

scrapper :: CardDef
scrapper =
  (asset "60511" "Scrapper" 2 Survivor)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#combat, #agility]
    }

cherishedKeepsake1 :: CardDef
cherishedKeepsake1 =
  (asset "60520" "Cherished Keepsake" 0 Survivor)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSlots = [#accessory]
    , cdSkills = [#willpower]
    , cdLevel = 1
    }

leatherCoat1 :: CardDef
leatherCoat1 =
  (asset "60521" "Leather Coat" 0 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Armor]
    , cdSlots = [#body]
    , cdLevel = 1
    }

eighteenDerringer2 :: CardDef
eighteenDerringer2 =
  (asset "60522" ".18 Derringer" 2 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdUses = uses Ammo 3
    , cdSlots = [#hand]
    , cdLevel = 2
    }

grannyOrne3 :: CardDef
grannyOrne3 =
  (asset "60527" ("Granny Orne" <:> "Tough Old Bird") 4 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Ally
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = 3
    }

chainsaw4 :: CardDef
chainsaw4 =
  (asset "60529" "Chainsaw" 4 Survivor)
    { cdSkills = [#combat, #combat, #combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdUses = uses Supply 3
    , cdSlots = [#hand, #hand]
    , cdLevel = 4
    }

quickLearner4 :: CardDef
quickLearner4 =
  permanent
    $ (asset "60530" "Quick Learner" 0 Survivor)
      { cdCardTraits = singleton Condition
      , cdLevel = 4
      }

dejaVu5 :: CardDef
dejaVu5 =
  permanent
    $ (asset "60531" "Déjà Vu" 0 Survivor)
      { cdCardTraits = setFromList [Talent, Cursed]
      , cdLevel = 5
      }

ladyEsprit :: CardDef
ladyEsprit =
  (storyAsset "81019" ("Lady Esprit" <:> "Dangerous Bokor") 4 TheBayou)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdUnique = True
    , cdSlots = [#ally]
    }

bearTrap :: CardDef
bearTrap =
  (storyAsset "81020" "Bear Trap" 0 TheBayou)
    { cdCardTraits = setFromList [Trap]
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

fishingNet :: CardDef
fishingNet =
  (storyAsset "81021" "Fishing Net" 0 TheBayou)
    { cdCardTraits = setFromList [Trap]
    , cdCost = Nothing
    , cdCardType = EncounterAssetType
    }

monstrousTransformation :: CardDef
monstrousTransformation =
  fast
    $ (storyAsset "81030" "Monstrous Transformation" 0 CurseOfTheRougarou)
      { cdCardTraits = setFromList [Talent]
      }

maskedCarnevaleGoer_17 :: CardDef
maskedCarnevaleGoer_17 =
  (storyAsset "82017b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

maskedCarnevaleGoer_18 :: CardDef
maskedCarnevaleGoer_18 =
  (storyAsset "82018b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

maskedCarnevaleGoer_19 :: CardDef
maskedCarnevaleGoer_19 =
  (storyAsset "82019b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

maskedCarnevaleGoer_20 :: CardDef
maskedCarnevaleGoer_20 =
  (storyAsset "82020b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

innocentReveler :: CardDef
innocentReveler =
  (storyAssetWithMany "82021" "Innocent Reveler" 0 CarnevaleOfHorrors 3)
    { cdCardTraits = setFromList [Ally, Bystander, Carnevale]
    , cdCost = Nothing
    }

maskedCarnevaleGoer_21 :: CardDef
maskedCarnevaleGoer_21 =
  (storyAsset "82021b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    , cdCost = Nothing
    }

abbessAllegriaDiBiase :: CardDef
abbessAllegriaDiBiase =
  ( storyAsset
      "82022"
      ("Abbess Allegria Di Biase" <:> "Most Blessed")
      4
      CarnevaleOfHorrors
  )
    { cdCardTraits = setFromList [Ally, Believer]
    , cdUnique = True
    , cdSkills = [#willpower, #intellect, #wild]
    , cdSlots = [#ally]
    }

bauta :: CardDef
bauta =
  (storyAsset "82023" "Bauta" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#combat, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

medicoDellaPeste :: CardDef
medicoDellaPeste =
  (storyAsset "82024" "Medico Della Peste" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#willpower, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

pantalone :: CardDef
pantalone =
  (storyAsset "82025" "Pantalone" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#intellect, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

gildedVolto :: CardDef
gildedVolto =
  (storyAsset "82026" "Gilded Volto" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#agility, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

bloodstainedDagger :: CardDef
bloodstainedDagger =
  (storyAsset "84006" ("Bloodstained Dagger" <:> "The Murder Weapon") 1 MurderAtTheExcelsiorHotel)
    { cdCardTraits = setFromList [Item, Weapon, Melee, Cursed]
    , cdSkills = [#wild]
    , cdSlots = [#hand]
    , cdUnique = True
    }

sergeantMonroe :: CardDef
sergeantMonroe =
  (storyAsset "84008" ("Sergeant Monroe" <:> "Two Days Until Retirement") 5 MurderAtTheExcelsiorHotel)
    { cdCardTraits = setFromList [Ally, Police]
    , cdSkills = [#willpower, #combat, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    }

alienDevice :: CardDef
alienDevice =
  (storyAsset "84028" ("Alien Device" <:> "Machinations from Beyond") 0 AlienInterference)
    { cdCardTraits = setFromList [Lead, Extraterrestrial]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

managersKey :: CardDef
managersKey =
  (storyAsset "84031" ("Manager's Key" <:> "Stained by Blood") 0 ExcelsiorManagement)
    { cdCardTraits = setFromList [Lead, Key]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

tomeOfRituals :: CardDef
tomeOfRituals =
  (storyAsset "84034" ("Tome of Rituals" <:> "Blasphemous Volume") 0 DarkRituals)
    { cdCardTraits = setFromList [Lead, Tome]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

sinisterSolution :: CardDef
sinisterSolution =
  (storyAsset "84037" ("Sinister Solution" <:> "Vile Concoction") 0 VileExperiments)
    { cdCardTraits = setFromList [Lead, Science]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

timeWornLocket :: CardDef
timeWornLocket =
  (storyAsset "84040" ("Time-Worn Locket" <:> "Mournful Vision of the Past") 0 SinsOfThePast)
    { cdCardTraits = setFromList [Lead, Charm]
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    }

daisysToteBagAdvanced :: CardDef
daisysToteBagAdvanced =
  (asset "90002" "Daisy's Tote Bag" 2 Neutral)
    { cdSkills = [#willpower, #intellect, #wild, #wild]
    , cdCardTraits = setFromList [Item]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "01002"]
    }

theNecronomiconAdvanced :: CardDef
theNecronomiconAdvanced =
  (weakness "90003" ("The Necronomicon" <:> "John Dee Translation"))
    { cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    }

mollyMaxwell :: CardDef
mollyMaxwell =
  (asset "98017" ("Molly Maxwell" <:> "The Exotic Morgana") 3 Neutral)
    { cdCardTraits = setFromList [Ally, Assistant]
    , cdSkills = [#willpower, #agility, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "07004"]
    }

courage :: CardDef
courage =
  (asset "xcourage" "Courage" 0 Neutral) {cdCardTraits = singleton Courage}

intrepid :: CardDef
intrepid =
  (asset "xintrepid" "Intrepid" 0 Guardian) {cdCardTraits = singleton Innate}
