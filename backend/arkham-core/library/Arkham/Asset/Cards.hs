module Arkham.Asset.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses hiding ( Key )
import Arkham.Asset.Uses qualified as Uses
import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding ( Dunwich )
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Name
import Arkham.SkillType ()
import Arkham.Slot
import Arkham.Token qualified as Token
import Arkham.Trait hiding ( Supply )

storyAsset :: CardCode -> Name -> Int -> EncounterSet -> CardDef 'AssetType
storyAsset cardCode name cost encounterSet =
  baseAsset (Just (encounterSet, 1)) cardCode name cost Neutral

storyAssetWithMany :: CardCode -> Name -> Int -> EncounterSet -> Int -> CardDef 'AssetType
storyAssetWithMany cardCode name cost encounterSet encounterSetCount =
  baseAsset (Just (encounterSet, encounterSetCount)) cardCode name cost Neutral

asset :: CardCode -> Name -> Int -> ClassSymbol -> CardDef 'AssetType
asset = baseAsset Nothing

setUses :: CardDef k -> Uses -> CardDef k
setUses cd u = cd { cdUses = u }

permanent :: CardDef k -> CardDef k
permanent cd = cd { cdPermanent = True, cdCost = Nothing }

fast :: CardDef k -> CardDef k
fast cd = cd { cdFastWindow = Just (DuringTurn You) }

weakness :: CardCode -> Name -> CardDef 'AssetType
weakness cardCode name = (baseAsset Nothing cardCode name 0 Neutral)
  { cdCardSubType = Just Weakness
  , cdRevelation = True
  , cdCost = Nothing
  }

storyWeakness :: CardCode -> Name -> EncounterSet -> CardDef 'AssetType
storyWeakness cardCode name encounterSet =
  (baseAsset (Just (encounterSet, 1)) cardCode name 0 Neutral)
    { cdCardSubType = Just Weakness
    , cdRevelation = True
    , cdCost = Nothing
    }

baseAsset
  :: Maybe (EncounterSet, Int)
  -> CardCode
  -> Name
  -> Int
  -> ClassSymbol
  -> CardDef 'AssetType
baseAsset mEncounterSet cardCode name cost classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Just (StaticCost cost)
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardSubType = Nothing
  , cdClassSymbols = singleton classSymbol
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdActions = []
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdVengeancePoints = Nothing
  , cdCriteria = mempty
  , cdOverrideActionPlayableIfCriteriaMet = False
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = fst <$> mEncounterSet
  , cdEncounterSetQuantity = snd <$> mEncounterSet
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  , cdUses = NoUses
  , cdPlayableFromDiscard = False
  , cdStage = Nothing
  , cdSlots = []
  , cdCardInHandEffects = False
  , cdCardInDiscardEffects = False
  , cdCardInSearchEffects = False
  , cdAlternateCardCodes = []
  , cdArt = unCardCode cardCode
  , cdLocationSymbol = Nothing
  , cdLocationRevealedSymbol = Nothing
  , cdLocationConnections = []
  , cdLocationRevealedConnections = []
  , cdPurchaseMentalTrauma = Nothing
  , cdCanReplace = True
  }

allPlayerAssetCards :: HashMap CardCode (CardDef 'AssetType)
allPlayerAssetCards = mapFromList $ concatMap
  toCardCodePairs
  [ abbessAllegriaDiBiase
  , aceOfRods1
  , aceOfSwords1
  , adaptable1
  , alchemicalTransmutation
  , alejandroVela
  , alyssaGraham
  , analyticalMind
  , ancestralKnowledge3
  , ancientStone1
  , ancientStoneKnowledgeOfTheElders4
  , ancientStoneMindsInHarmony4
  , aquinnah1
  , aquinnah3
  , arcaneEnlightenment
  , arcaneInitiate
  , arcaneInitiate3
  , arcaneInsight4
  , arcaneResearch
  , arcaneStudies
  , arcaneStudies2
  , archaicGlyphs
  , archaicGlyphsGuidingStones3
  , archaicGlyphsProphecyForetold3
  , armorOfArdennes5
  , artStudent
  , backpack
  , bandolier
  , baronSamedi
  , baseballBat
  , beatCop
  , beatCop2
  , blackjack
  , bloodPact3
  , bookOfShadows1
  , bookOfShadows3
  , borrowedTime3
  , boxingGloves
  , boxingGloves3
  , brotherXavier1
  , bulletproofVest3
  , burglary
  , catBurglar1
  , celaenoFragments
  , chainsaw4
  , charisma3
  , charlesRossEsq
  , charonsObol1
  , cherishedKeepsake
  , cherishedKeepsake1
  , chicagoTypewriter4
  , clarityOfMind
  , claspOfBlackOnyx
  , coltVestPocket
  , coltVestPocket2
  , cornered2
  , combatTraining1
  , crystallineElderSign3
  , daisysToteBag
  , daisysToteBagAdvanced
  , darioElAmin
  , darkHorse
  , davidRenfield
  , deathXiii1
  , decoratedSkull
  , dejaVu5
  , detectivesColt1911s
  , digDeep
  , digDeep2
  , discOfItzamna
  , discOfItzamna2
  , drElliHorowitz
  , drFrancisMorgan
  , drHenryArmitage
  , drMilanChristopher
  , drWilliamTMaleson
  , drawingThin
  , duke
  , earlSawyer
  , eighteenDerringer
  , eighteenDerringer2
  , elderSignAmulet3
  , encyclopedia
  , encyclopedia2
  , esotericAtlas2
  , esotericFormula
  , expeditionJournal
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
  , fortyFiveAutomatic
  , fortyFiveAutomatic2
  , fortyOneDerringer
  , fortyOneDerringer2
  , fourOfCups1
  , grannyOrne
  , grannyOrne3
  , gravediggersShovel
  , greteWagner
  , greteWagner3
  , grimmsFairyTales
  , grotesqueStatue4
  , grounded1
  , guardDog
  , guardDog2
  , handcuffs
  , handcuffs2
  , harlanEarnstone
  , hardKnocks
  , hardKnocks2
  , heirloomOfHyperborea
  , hemisphericMap2
  , henryDeveau
  , highRoller2
  , higherEducation
  , higherEducation3
  , hiredMuscle1
  , holyRosary
  , hyperawareness
  , hyperawareness2
  , hypnoticTherapy
  , ichtacaTheForgottenGuardian
  , innocentReveler
  , inTheKnow1
  , jakeWilliams
  , jennysTwin45s
  , jewelOfAureolus3
  , jimsTrumpet
  , joeyTheRatVigil
  , keenEye
  , keenEye3
  , kerosene1
  , keyOfYs
  , knife
  , knuckleduster
  , kukri
  , laboratoryAssistant
  , ladyEsprit
  , lantern
  , leatherCoat
  , leatherCoat1
  , leoDeLuca
  , leoDeLuca1
  , libraryDocent1
  , lightningGun5
  , liquidCourage
  , litaChantler
  , livreDeibon
  , lockpicks
  , lockpicks1
  , lolaSantiago3
  , loneWolf
  , luckyCigaretteCase
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
  , meatCleaver
  , medicalTexts
  , miskatonicArchaeologyFunding4
  , mistsOfRlyeh
  , mistsOfRlyeh2
  , mistsOfRlyeh4
  , mitchBrown
  , monstrousTransformation
  , moxie1
  , mysteriousRaven
  , newspaper
  , newspaper2
  , occultLexicon
  , oldBookOfLore
  , oldBookOfLore3
  , oldHuntingRifle3
  , oldKeyring
  , oliveMcBride
  , onYourOwn3
  , ornateBow3
  , otherwordlyCompass2
  , painkillers
  , pathfinder1
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
  , professorWarrenRice
  , protectiveIncantation1
  , quickLearner4
  , quickStudy2
  , rabbitsFoot
  , rabbitsFoot3
  , randallCho
  , recallTheFuture2
  , relentless
  , relicHunter3
  , relicOfAgesADeviceOfSomeSort
  , relicOfAgesForestallingTheFuture
  , relicOfAgesRepossessThePast
  , relicOfAgesUnleashTheTimestream
  , researchLibrarian
  , riteOfSeeking
  , riteOfSeeking2
  , riteOfSeeking4
  , ritualCandles
  , rolands38Special
  , safeguard
  , scavenging
  , scientificTheory1
  , scrapper
  , scrapper3
  , scrollOfProphecies
  , scrying
  , scrying3
  , sealOfTheSeventhSign5
  , shardsOfTheVoid3
  , shotgun4
  , shrewdAnalysis
  , shrivelling
  , shrivelling3
  , shrivelling5
  , smokingPipe
  , sophieInLovingMemory
  , sophieItWasAllMyFault
  , spiritAthame1
  , stealth
  , stickToThePlan3
  , streetwise
  , streetwise3
  , studious3
  , songOfTheDead2
  , spiritSpeaker
  , springfieldM19034
  , stHubertsKey
  , strangeSolution
  , strangeSolutionAcidicIchor4
  , strangeSolutionFreezingVariant4
  , strangeSolutionRestorativeConcoction4
  , strayCat
  , suggestion4
  , survivalKnife
  , switchblade
  , switchblade2
  , theChthonianStone
  , theCodexOfAges
  , theCustodian
  , theGoldPocketWatch4
  , theKingInYellow
  , theMoonXiii1
  , theNecronomicon
  , theNecronomiconAdvanced
  , theNecronomiconOlausWormiusTranslation
  , theNecronomiconPetrusDeDaciaTranslation5
  , thePallidMask
  , theRedGlovedMan5
  , theSkeletonKey2
  , theTatteredCloak
  , theTowerXVI
  , thermos
  , thirtyTwoColt
  , timewornBrand5
  , toothOfEztli
  , trackShoes
  , treasureHunter1
  , trenchCoat
  , trenchKnife
  , trueGrit
  , tryAndTryAgain1
  , tryAndTryAgain3
  , twilightBlade
  , untilTheEndOfTime
  , vaultOfKnowledge
  , venturer
  , wellConnected
  , wellConnected3
  , wellPrepared2
  , wendysAmulet
  , whittonGreene
  , whittonGreene2
  , yaotl1
  , zebulonWhateley
  , zoeysCross
  ]

allEncounterAssetCards :: HashMap CardCode (CardDef 'AssetType)
allEncounterAssetCards = mapFromList $ map
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
  ]

allSpecialPlayerAssetCards :: HashMap CardCode (CardDef 'AssetType)
allSpecialPlayerAssetCards =
  mapFrom toCardCode [courage, intrepid]

allSpecialEncounterAssetCards :: HashMap CardCode (CardDef 'AssetType)
allSpecialEncounterAssetCards =
  mapFrom toCardCode [straitjacket]

rolands38Special :: CardDef 'AssetType
rolands38Special = (asset "01006" "Roland's .38 Special" 3 Neutral)
  { cdSkills = [#combat, #agility, #wild]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdUnique = True
  , cdUses = Uses Ammo 4
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01506"]
  }

daisysToteBag :: CardDef 'AssetType
daisysToteBag = (asset "01008" "Daisy's Tote Bag" 2 Neutral)
  { cdSkills = [#willpower, #intellect, #wild]
  , cdCardTraits = setFromList [Item]
  , cdUnique = True
  , cdAlternateCardCodes = ["01508"]
  }

theNecronomicon :: CardDef 'AssetType
theNecronomicon =
  (weakness "01009" ("The Necronomicon" <:> "John Dee Translation"))
    { cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [HandSlot]
    , cdAlternateCardCodes = ["01509"]
    }

heirloomOfHyperborea :: CardDef 'AssetType
heirloomOfHyperborea = (asset
                         "01012"
                         ("Heirloom of Hyperborea"
                         <:> "Artifact from Another Life"
                         )
                         3
                         Neutral
                       )
  { cdSkills = [#willpower, #combat, #wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdUnique = True
  , cdSlots = [AccessorySlot]
  , cdAlternateCardCodes = ["01512"]
  }

wendysAmulet :: CardDef 'AssetType
wendysAmulet = (asset "01014" "Wendy's Amulet" 2 Neutral)
  { cdSkills = [#wild, #wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdUnique = True
  , cdSlots = [AccessorySlot]
  , cdAlternateCardCodes = ["01514"]
  }

fortyFiveAutomatic :: CardDef 'AssetType
fortyFiveAutomatic = (asset "01016" ".45 Automatic" 4 Guardian)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdUses = Uses Ammo 4
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01516"]
  }

physicalTraining :: CardDef 'AssetType
physicalTraining = (asset "01017" "Physical Training" 2 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Talent]
  , cdAlternateCardCodes = ["01517", "60108"]
  }

beatCop :: CardDef 'AssetType
beatCop = (asset "01018" "Beat Cop" 4 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Ally, Police]
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01518"]
  }

firstAid :: CardDef 'AssetType
firstAid = (asset "01019" "First Aid" 2 Guardian)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Talent, Science]
  , cdUses = Uses Supply 3
  , cdAlternateCardCodes = ["01519"]
  }

machete :: CardDef 'AssetType
machete = (asset "01020" "Machete" 3 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01520"]
  }

guardDog :: CardDef 'AssetType
guardDog = (asset "01021" "Guard Dog" 3 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Ally, Creature]
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01521"]
  }

policeBadge2 :: CardDef 'AssetType
policeBadge2 = (asset "01027" "Police Badge" 3 Guardian)
  { cdSkills = [#willpower, #wild]
  , cdCardTraits = setFromList [Item]
  , cdLevel = 2
  , cdSlots = [AccessorySlot]
  , cdAlternateCardCodes = ["01527"]
  }

beatCop2 :: CardDef 'AssetType
beatCop2 = (asset "01028" "Beat Cop" 4 Guardian)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Ally, Police]
  , cdLevel = 2
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01528"]
  }

shotgun4 :: CardDef 'AssetType
shotgun4 = (asset "01029" "Shotgun" 5 Guardian)
  { cdSkills = [#combat, #combat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdLevel = 4
  , cdUses = Uses Ammo 2
  , cdSlots = [HandSlot, HandSlot]
  , cdAlternateCardCodes = ["01529"]
  }

magnifyingGlass :: CardDef 'AssetType
magnifyingGlass = fast $ (asset "01030" "Magnifying Glass" 1 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tool]
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01530"]
  }

oldBookOfLore :: CardDef 'AssetType
oldBookOfLore = (asset "01031" "Old Book of Lore" 3 Seeker)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Tome]
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01531"]
  }

researchLibrarian :: CardDef 'AssetType
researchLibrarian = (asset "01032" "Research Librarian" 2 Seeker)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01532"]
  }

drMilanChristopher :: CardDef 'AssetType
drMilanChristopher =
  (asset
      "01033"
      ("Dr. Milan Christopher" <:> "Professor of Entomology")
      4
      Seeker
    )
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [AllySlot]
    , cdAlternateCardCodes = ["01533"]
    }

hyperawareness :: CardDef 'AssetType
hyperawareness = (asset "01034" "Hyperawareness" 2 Seeker)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Talent]
  , cdAlternateCardCodes = ["01534"]
  }

medicalTexts :: CardDef 'AssetType
medicalTexts = (asset "01035" "Medical Texts" 2 Seeker)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Tome]
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01535"]
  }

magnifyingGlass1 :: CardDef 'AssetType
magnifyingGlass1 = fast $ (asset "01040" "Magnifying Glass" 0 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tool]
  , cdLevel = 1
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01540"]
  }

discOfItzamna2 :: CardDef 'AssetType
discOfItzamna2 =
  (asset "01041" ("Disc of Itzamna" <:> "Protective Amulet") 3 Seeker)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 2
    , cdUnique = True
    , cdSlots = [AccessorySlot]
    , cdAlternateCardCodes = ["01541"]
    }

encyclopedia2 :: CardDef 'AssetType
encyclopedia2 = (asset "01042" "Encyclopedia" 2 Seeker)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Item, Tome]
  , cdLevel = 2
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01542"]
  }

switchblade :: CardDef 'AssetType
switchblade = fast $ (asset "01044" "Switchblade" 1 Rogue)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01544", "60307"]
  }

burglary :: CardDef 'AssetType
burglary = (asset "01045" "Burglary" 1 Rogue)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Talent, Illicit]
  , cdAlternateCardCodes = ["01545"]
  }

pickpocketing :: CardDef 'AssetType
pickpocketing = (asset "01046" "Pickpocketing" 2 Rogue)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Talent, Illicit]
  , cdAlternateCardCodes = ["01546"]
  }

fortyOneDerringer :: CardDef 'AssetType
fortyOneDerringer = (asset "01047" ".41 Derringer" 3 Rogue)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdUses = Uses Ammo 3
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01547"]
  }

leoDeLuca :: CardDef 'AssetType
leoDeLuca = (asset "01048" ("Leo De Luca" <:> "The Louisiana Lion") 6 Rogue)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Ally, Criminal]
  , cdUnique = True
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01548"]
  }

hardKnocks :: CardDef 'AssetType
hardKnocks = (asset "01049" "Hard Knocks" 2 Rogue)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Talent]
  , cdAlternateCardCodes = ["01549"]
  }

leoDeLuca1 :: CardDef 'AssetType
leoDeLuca1 = (asset "01054" ("Leo De Luca" <:> "The Louisiana Lion") 5 Rogue)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Ally, Criminal]
  , cdLevel = 1
  , cdUnique = True
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01554"]
  }

catBurglar1 :: CardDef 'AssetType
catBurglar1 = (asset "01055" "Cat Burglar" 4 Rogue)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Ally, Criminal]
  , cdLevel = 1
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01555"]
  }

forbiddenKnowledge :: CardDef 'AssetType
forbiddenKnowledge = (asset "01058" "Forbidden Knowledge" 0 Mystic)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Talent]
  , cdUses = Uses Secret 4
  , cdAlternateCardCodes = ["01558"]
  }

holyRosary :: CardDef 'AssetType
holyRosary = (asset "01059" "Holy Rosary" 2 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Charm]
  , cdSlots = [AccessorySlot]
  , cdAlternateCardCodes = ["01559"]
  }

shrivelling :: CardDef 'AssetType
shrivelling = (asset "01060" "Shrivelling" 3 Mystic)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Spell]
  , cdUses = Uses Charge 4
  , cdSlots = [ArcaneSlot]
  , cdAlternateCardCodes = ["01560"]
  }

scrying :: CardDef 'AssetType
scrying = (asset "01061" "Scrying" 1 Mystic)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Spell]
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  , cdAlternateCardCodes = ["01561"]
  }

arcaneStudies :: CardDef 'AssetType
arcaneStudies = (asset "01062" "Arcane Studies" 2 Mystic)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Talent]
  , cdAlternateCardCodes = ["01562"]
  }

arcaneInitiate :: CardDef 'AssetType
arcaneInitiate = (asset "01063" "Arcane Initiate" 1 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Ally, Sorcerer]
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01563"]
  }

bookOfShadows3 :: CardDef 'AssetType
bookOfShadows3 = (asset "01070" "Book of Shadows" 4 Mystic)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Item, Tome]
  , cdLevel = 3
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01570"]
  }

grotesqueStatue4 :: CardDef 'AssetType
grotesqueStatue4 = (asset "01071" "Grotesque Statue" 2 Mystic)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdLevel = 4
  , cdUses = Uses Charge 4
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01571"]
  }

leatherCoat :: CardDef 'AssetType
leatherCoat = (asset "01072" "Leather Coat" 0 Survivor)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Armor]
  , cdSlots = [BodySlot]
  , cdAlternateCardCodes = ["01572"]
  }

scavenging :: CardDef 'AssetType
scavenging = (asset "01073" "Scavenging" 1 Survivor)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Talent]
  , cdAlternateCardCodes = ["01573"]
  }

baseballBat :: CardDef 'AssetType
baseballBat = (asset "01074" "Baseball Bat" 2 Survivor)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot, HandSlot]
  , cdAlternateCardCodes = ["01574"]
  }

rabbitsFoot :: CardDef 'AssetType
rabbitsFoot = (asset "01075" "Rabbit's Foot" 1 Survivor)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Item, Charm]
  , cdSlots = [AccessorySlot]
  , cdAlternateCardCodes = ["01575", "60510"]
  }

strayCat :: CardDef 'AssetType
strayCat = (asset "01076" "Stray Cat" 1 Survivor)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Ally, Creature]
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01576"]
  }

digDeep :: CardDef 'AssetType
digDeep = (asset "01077" "Dig Deep" 2 Survivor)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Talent]
  , cdAlternateCardCodes = ["01577"]
  }

aquinnah1 :: CardDef 'AssetType
aquinnah1 = (asset "01082" ("Aquinnah" <:> "The Forgotten Daughter") 5 Survivor
            )
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Ally]
  , cdLevel = 1
  , cdUnique = True
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01582"]
  }

knife :: CardDef 'AssetType
knife = (asset "01086" "Knife" 1 Neutral)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01586"]
  }

flashlight :: CardDef 'AssetType
flashlight = (asset "01087" "Flashlight" 2 Neutral)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tool]
  , cdUses = Uses Supply 3
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01587"]
  }

bulletproofVest3 :: CardDef 'AssetType
bulletproofVest3 = (asset "01094" "Bulletproof Vest" 2 Neutral)
  { cdSkills = [#combat, #wild]
  , cdCardTraits = setFromList [Item, Armor]
  , cdLevel = 3
  , cdSlots = [BodySlot]
  , cdAlternateCardCodes = ["01594"]
  }

elderSignAmulet3 :: CardDef 'AssetType
elderSignAmulet3 = (asset "01095" "Elder Sign Amulet" 2 Neutral)
  { cdSkills = [#willpower, #wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdLevel = 3
  , cdSlots = [AccessorySlot]
  , cdAlternateCardCodes = ["01595"]
  }

litaChantler :: CardDef 'AssetType
litaChantler =
  (storyAsset "01117" ("Lita Chantler" <:> "The Zealot") 0 TheGathering)
    { cdCardTraits = setFromList [Ally]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

zoeysCross :: CardDef 'AssetType
zoeysCross =
  (asset "02006" ("Zoey's Cross" <:> "Symbol of Righteousness") 1 Neutral)
    { cdSkills = [#combat, #combat, #wild]
    , cdCardTraits = setFromList [Item, Charm]
    , cdUnique = True
    , cdSlots = [AccessorySlot]
    }

jennysTwin45s :: CardDef 'AssetType
jennysTwin45s =
  (asset "02010" ("Jenny's Twin .45s" <:> "A Perfect Fit") 0 Neutral)
    { cdSkills = [#agility, #agility, #wild]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdCost = Just DynamicCost
    , cdUnique = True
    , cdSlots = [HandSlot, HandSlot]
    }

jimsTrumpet :: CardDef 'AssetType
jimsTrumpet = (asset "02012" ("Jim's Trumpet" <:> "The Dead Listen") 2 Neutral)
  { cdSkills = [#willpower, #willpower, #wild]
  , cdCardTraits = setFromList [Item, Instrument, Relic]
  , cdUnique = True
  , cdSlots = [HandSlot]
  }

duke :: CardDef 'AssetType
duke = (asset "02014" ("Duke" <:> "Loyal Hound") 2 Neutral)
  { cdCardTraits = setFromList [Ally, Creature]
  , cdUnique = True
  }

blackjack :: CardDef 'AssetType
blackjack = (asset "02016" "Blackjack" 1 Guardian)
  { cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSkills = [#combat]
  , cdSlots = [HandSlot]
  }

laboratoryAssistant :: CardDef 'AssetType
laboratoryAssistant = (asset "02020" "Laboratory Assistant" 2 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Ally, Miskatonic, Science]
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["60212"]
  }

strangeSolution :: CardDef 'AssetType
strangeSolution =
  (asset "02021" ("Strange Solution" <:> "Unidentified") 1 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Item, Science]
    }

liquidCourage :: CardDef 'AssetType
liquidCourage = (asset "02024" "Liquid Courage" 1 Rogue)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Illicit]
  , cdUses = Uses Supply 4
  }

hiredMuscle1 :: CardDef 'AssetType
hiredMuscle1 = (asset "02027" "Hired Muscle" 1 Rogue)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Ally, Criminal]
  , cdLevel = 1
  , cdSlots = [AllySlot]
  }

riteOfSeeking :: CardDef 'AssetType
riteOfSeeking = (asset "02028" "Rite of Seeking" 4 Mystic)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Spell]
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  }

ritualCandles :: CardDef 'AssetType
ritualCandles = (asset "02029" "Ritual Candles" 1 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Item
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["60405"]
  }

clarityOfMind :: CardDef 'AssetType
clarityOfMind = (asset "02030" "Clarity of Mind" 2 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Spell
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  }

fireAxe :: CardDef 'AssetType
fireAxe = (asset "02032" "Fire Axe" 1 Survivor)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot]
  }

peterSylvestre :: CardDef 'AssetType
peterSylvestre =
  (asset "02033" ("Peter Sylvestre" <:> "Big Man on Campus") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

peterSylvestre2 :: CardDef 'AssetType
peterSylvestre2 =
  (asset "02035" ("Peter Sylvestre" <:> "Big Man on Campus") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdLevel = 2
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

kukri :: CardDef 'AssetType
kukri = (asset "02036" "Kukri" 2 Neutral)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot]
  }

drHenryArmitage :: CardDef 'AssetType
drHenryArmitage = (storyAsset
                    "02040"
                    ("Dr. Henry Armitage" <:> "The Head Librarian")
                    2
                    ArmitagesFate
                  )
  { cdSkills = [#wild, #wild]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

alchemicalConcoction :: CardDef 'AssetType
alchemicalConcoction =
  (storyAsset "02059" "Alchemical Concoction" 0 ExtracurricularActivity)
    { cdCardTraits = setFromList [Item, Science]
    }

jazzMulligan :: CardDef 'AssetType
jazzMulligan = (storyAsset
                 "02060"
                 ("\"Jazz\" Mulligan" <:> "The Head Janitor")
                 0
                 ExtracurricularActivity
               )
  { cdCardTraits = setFromList [Ally, Miskatonic]
  , cdUnique = True
  }

professorWarrenRice :: CardDef 'AssetType
professorWarrenRice = (storyAsset
                        "02061"
                        ("Professor Warren Rice" <:> "Professor of Languages")
                        3
                        ExtracurricularActivity
                      )
  { cdSkills = [#intellect, #wild]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

peterClover :: CardDef 'AssetType
peterClover = (storyAsset
                "02079"
                ("Peter Clover" <:> "Holding All the Cards")
                0
                TheHouseAlwaysWins
              )
  { cdCardTraits = setFromList [Humanoid, Criminal]
  , cdUnique = True
  }

drFrancisMorgan :: CardDef 'AssetType
drFrancisMorgan = (storyAsset
                    "02080"
                    ("Dr. Francis Morgan" <:> "Professor of Archaeology")
                    3
                    TheHouseAlwaysWins
                  )
  { cdSkills = [#combat, #wild]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

brotherXavier1 :: CardDef 'AssetType
brotherXavier1 =
  (asset "02106" ("Brother Xavier" <:> "Pure of Spirit") 5 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally]
    , cdLevel = 1
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

pathfinder1 :: CardDef 'AssetType
pathfinder1 = (asset "02108" "Pathfinder" 3 Seeker)
  { cdSkills = [#agility]
  , cdCardTraits = singleton Talent
  , cdLevel = 1
  }

adaptable1 :: CardDef 'AssetType
adaptable1 = permanent $ (asset "02110" "Adaptable" 0 Rogue)
  { cdCardTraits = setFromList [Talent]
  , cdLevel = 1
  }

songOfTheDead2 :: CardDef 'AssetType
songOfTheDead2 = (asset "02112" "Song of the Dead" 2 Mystic)
  { cdCardTraits = setFromList [Spell, Song]
  , cdSkills = [#willpower]
  , cdLevel = 2
  , cdUses = Uses Charge 5
  , cdSlots = [ArcaneSlot]
  }

fireExtinguisher1 :: CardDef 'AssetType
fireExtinguisher1 = (asset "02114" "Fire Extinguisher" 2 Survivor)
  { cdCardTraits = setFromList [Item, Tool, Melee]
  , cdSkills = [#agility]
  , cdLevel = 1
  , cdSlots = [HandSlot]
  }

smokingPipe :: CardDef 'AssetType
smokingPipe = (asset "02116" "Smoking Pipe" 1 Neutral)
  { cdCardTraits = singleton Item
  , cdSkills = [#willpower]
  , cdUses = Uses Supply 3
  }

painkillers :: CardDef 'AssetType
painkillers = (asset "02117" "Painkillers" 1 Neutral)
  { cdCardTraits = singleton Item
  , cdSkills = [#willpower]
  , cdUses = Uses Supply 3
  }

haroldWalsted :: CardDef 'AssetType
haroldWalsted = (storyAsset
                  "02138"
                  ("Harold Walsted" <:> "Curator of the Museum")
                  0
                  TheMiskatonicMuseum
                )
  { cdCardTraits = setFromList [Ally, Miskatonic]
  , cdUnique = True
  }

adamLynch :: CardDef 'AssetType
adamLynch =
  (storyAsset "02139" ("Adam Lynch" <:> "Museum Security") 0 TheMiskatonicMuseum
    )
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    }

theNecronomiconOlausWormiusTranslation :: CardDef 'AssetType
theNecronomiconOlausWormiusTranslation =
  (storyAsset
      "02140"
      ("The Necronomicon" <:> "Olaus Wormius Translation")
      2
      TheMiskatonicMuseum
    )
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [HandSlot]
    }

bandolier :: CardDef 'AssetType
bandolier = (asset "02147" "Bandolier" 2 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item]
  , cdSlots = [BodySlot]
  }

artStudent :: CardDef 'AssetType
artStudent = (asset "02149" "Art Student" 2 Seeker)
  { cdCardTraits = setFromList [Ally, Miskatonic]
  , cdSkills = [#intellect]
  , cdSlots = [AllySlot]
  }

switchblade2 :: CardDef 'AssetType
switchblade2 = fast $ (asset "02152" "Switchblade" 1 Rogue)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
  , cdLevel = 2
  , cdSlots = [HandSlot]
  }

shrivelling3 :: CardDef 'AssetType
shrivelling3 = (asset "02154" "Shrivelling" 3 Mystic)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = singleton Spell
  , cdLevel = 3
  , cdUses = Uses Charge 4
  , cdSlots = [ArcaneSlot]
  }

newspaper :: CardDef 'AssetType
newspaper = (asset "02155" "Newspaper" 1 Survivor)
  { cdSkills = [#intellect]
  , cdCardTraits = singleton Item
  , cdSlots = [HandSlot]
  }

relicHunter3 :: CardDef 'AssetType
relicHunter3 = permanent $ (asset "02157" "Relic Hunter" 0 Neutral)
  { cdCardTraits = singleton Talent
  , cdLevel = 3
  , cdAlternateCardCodes = ["01695"]
  }

charisma3 :: CardDef 'AssetType
charisma3 = permanent $ (asset "02158" "Charisma" 0 Neutral)
  { cdCardTraits = singleton Talent
  , cdLevel = 3
  , cdAlternateCardCodes = ["01694"]
  }

helplessPassenger :: CardDef 'AssetType
helplessPassenger =
  (storyAsset "02179" "Helpless Passenger" 0 TheEssexCountyExpress)
    { cdCardTraits = setFromList [Ally, Bystander]
    , cdKeywords = singleton Keyword.Surge
    , cdEncounterSetQuantity = Just 3
    }

keenEye3 :: CardDef 'AssetType
keenEye3 = permanent $ (asset "02185" "Keen Eye" 0 Guardian)
  { cdCardTraits = setFromList [Talent]
  , cdLevel = 3
  }

higherEducation3 :: CardDef 'AssetType
higherEducation3 = permanent $ (asset "02187" "Higher Education" 0 Seeker)
  { cdCardTraits = setFromList [Talent]
  , cdLevel = 3
  }

loneWolf :: CardDef 'AssetType
loneWolf = (asset "02188" "Lone Wolf" 1 Rogue)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Talent]
  , cdLimits = [LimitPerInvestigator 1]
  }

streetwise3 :: CardDef 'AssetType
streetwise3 = permanent $ (asset "02189" "Streetwise" 0 Rogue)
  { cdCardTraits = setFromList [Talent]
  , cdLevel = 3
  }

bloodPact3 :: CardDef 'AssetType
bloodPact3 = permanent $ (asset "02191" "Blood Pact" 0 Mystic)
  { cdCardTraits = setFromList [Spell, Pact]
  , cdLevel = 3
  }

scrapper3 :: CardDef 'AssetType
scrapper3 = permanent $ (asset "02193" "Scrapper" 0 Survivor)
  { cdCardTraits = setFromList [Talent]
  , cdLevel = 3
  }

keyToTheChamber :: CardDef 'AssetType
keyToTheChamber = (storyAsset "02215" "Key to the Chamber" 0 BloodOnTheAltar)
  { cdCardTraits = setFromList [Item, Key]
  , cdUnique = True
  }

zebulonWhateley :: CardDef 'AssetType
zebulonWhateley = (storyAsset
                    "02217"
                    ("Zebulon Whateley" <:> "Recalling Ancient Things")
                    3
                    BloodOnTheAltar
                  )
  { cdCardTraits = setFromList [Ally, Dunwich]
  , cdSkills = [#willpower, #wild]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

earlSawyer :: CardDef 'AssetType
earlSawyer = (storyAsset
               "02218"
               ("Earl Sawyer" <:> "Smarter Than He Lets On")
               3
               BloodOnTheAltar
             )
  { cdCardTraits = setFromList [Ally, Dunwich]
  , cdSkills = [#agility, #wild]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

powderOfIbnGhazi :: CardDef 'AssetType
powderOfIbnGhazi = (storyAsset
                     "02219"
                     ("Powder of Ibn-Ghazi" <:> "Seeing Things Unseen")
                     0
                     BloodOnTheAltar
                   )
  { cdCardTraits = singleton Item
  }

springfieldM19034 :: CardDef 'AssetType
springfieldM19034 = (asset "02226" "Springfield M1903" 4 Guardian)
  { cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdLevel = 4
  , cdSkills = [#combat, #agility]
  , cdUses = Uses Ammo 3
  , cdSlots = [HandSlot, HandSlot]
  }

luckyDice2 :: CardDef 'AssetType
luckyDice2 = (asset "02230" ("Lucky Dice" <:> "... Or Are They?") 2 Rogue)
  { cdCardTraits = setFromList [Item, Relic]
  , cdSkills = [#willpower, #agility]
  , cdExceptional = True
  , cdLevel = 2
  , cdSlots = [AccessorySlot]
  }

alyssaGraham :: CardDef 'AssetType
alyssaGraham =
  (asset "02232" ("Alyssa Graham" <:> "Speaker to the Dead") 4 Mystic)
    { cdCardTraits = setFromList [Ally, Sorcerer]
    , cdSkills = [#intellect]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

riteOfSeeking4 :: CardDef 'AssetType
riteOfSeeking4 = (asset "02233" "Rite of Seeking" 5 Mystic)
  { cdCardTraits = singleton Spell
  , cdSkills = [#intellect, #intellect]
  , cdLevel = 4
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  }

darkHorse :: CardDef 'AssetType
darkHorse = (asset "02234" "Dark Horse" 3 Survivor)
  { cdCardTraits = singleton Condition
  , cdSkills = [#willpower]
  , cdLimits = [LimitPerInvestigator 1]
  }

esotericFormula :: CardDef 'AssetType
esotericFormula =
  (storyAsset "02254" "Esoteric Formula" 0 UndimensionedAndUnseen)
    { cdCardTraits = singleton Spell
    , cdEncounterSetQuantity = Just 4
    }

strangeSolutionRestorativeConcoction4 :: CardDef 'AssetType
strangeSolutionRestorativeConcoction4 =
  (asset "02262" ("Strange Solution" <:> "Restorative Concoction") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#willpower, #willpower]
    , cdLevel = 4
    , cdUses = Uses Supply 4
    }

strangeSolutionAcidicIchor4 :: CardDef 'AssetType
strangeSolutionAcidicIchor4 =
  (asset "02263" ("Strange Solution" <:> "Acidic Ichor") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#combat, #combat]
    , cdLevel = 4
    , cdUses = Uses Supply 4
    }

strangeSolutionFreezingVariant4 :: CardDef 'AssetType
strangeSolutionFreezingVariant4 =
  (asset "02264" ("Strange Solution" <:> "Freezing Variant") 1 Seeker)
    { cdCardTraits = setFromList [Item, Science]
    , cdSkills = [#agility, #agility]
    , cdLevel = 4
    , cdUses = Uses Supply 4
    }

joeyTheRatVigil :: CardDef 'AssetType
joeyTheRatVigil =
  (asset "02265" ("Joey \"The Rat\" Vigil" <:> "Lookin' Out for #1") 4 Rogue)
    { cdCardTraits = setFromList [Ally, Criminal]
    , cdSkills = [#intellect, #agility]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

jewelOfAureolus3 :: CardDef 'AssetType
jewelOfAureolus3 =
  (asset "02269" ("Jewel of Aureolus" <:> "Gift of the Homunculi") 3 Mystic)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#wild]
    , cdLevel = 3
    , cdUnique = True
    , cdSlots = [AccessorySlot]
    }

fineClothes :: CardDef 'AssetType
fineClothes = (asset "02272" "Fine Clothes" 1 Neutral)
  { cdCardTraits = setFromList [Item, Clothing]
  , cdSkills = [#agility]
  , cdSlots = [BodySlot]
  }

lightningGun5 :: CardDef 'AssetType
lightningGun5 = (asset "02301" "Lightning Gun" 6 Guardian)
  { cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdLevel = 5
  , cdSkills = [#intellect, #combat]
  , cdUses = Uses Ammo 3
  , cdSlots = [HandSlot, HandSlot]
  }

drWilliamTMaleson :: CardDef 'AssetType
drWilliamTMaleson = (asset
                      "02302"
                      ("Dr. William T. Maleson" <:> "Working on Something Big")
                      1
                      Seeker
                    )
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

chicagoTypewriter4 :: CardDef 'AssetType
chicagoTypewriter4 = (asset "02304" "Chicago Typewriter" 5 Rogue)
  { cdSkills = [#combat, #combat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdLevel = 4
  , cdUses = Uses Ammo 4
  , cdSlots = [HandSlot, HandSlot]
  }

theGoldPocketWatch4 :: CardDef 'AssetType
theGoldPocketWatch4 =
  (asset "02305" ("The Gold Pocket Watch" <:> "Stealing Time") 2 Rogue)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 4
    , cdUnique = True
    , cdExceptional = True
    , cdSlots = [AccessorySlot]
    }

shrivelling5 :: CardDef 'AssetType
shrivelling5 = (asset "02306" "Shrivelling" 3 Mystic)
  { cdSkills = [#willpower, #combat, #combat]
  , cdCardTraits = singleton Spell
  , cdLevel = 5
  , cdUses = Uses Charge 4
  , cdSlots = [ArcaneSlot]
  }


aquinnah3 :: CardDef 'AssetType
aquinnah3 = (asset "02308" ("Aquinnah" <:> "The Forgotten Daughter") 4 Survivor
            )
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Ally]
  , cdLevel = 3
  , cdUnique = True
  , cdSlots = [AllySlot]
  , cdAlternateCardCodes = ["01691"]
  }

tryAndTryAgain3 :: CardDef 'AssetType
tryAndTryAgain3 = (asset "02309" "Try and Try Again" 2 Survivor)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Talent
  , cdLevel = 3
  }

theRedGlovedMan5 :: CardDef 'AssetType
theRedGlovedMan5 =
  fast
    $ (asset "02310" ("The Red-Gloved Man" <:> "He Was Never There") 2 Neutral)
        { cdSkills = [#wild]
        , cdCardTraits = setFromList [Ally, Conspirator]
        , cdLevel = 5
        , cdUnique = True
        , cdSlots = [AllySlot]
        }

sophieInLovingMemory :: CardDef 'AssetType
sophieInLovingMemory =
  (asset "03009" ("Sophie" <:> "In Loving Memory") 0 Neutral)
    { cdCardTraits = setFromList [Item, Spirit]
    , cdUnique = True
    , cdCost = Nothing
    }

sophieItWasAllMyFault :: CardDef 'AssetType
sophieItWasAllMyFault =
  (asset "03009b" ("Sophie" <:> "It Was All My Fault") 0 Neutral)
    { cdCardTraits = setFromList [Item, Madness]
    , cdUnique = True
    , cdCost = Nothing
    }

analyticalMind :: CardDef 'AssetType
analyticalMind =
  (asset "03010" ("Analytical Mind" <:> "Between the Lines") 3 Neutral)
    { cdCardTraits = singleton Talent
    , cdSkills = [#wild, #wild]
    }

theKingInYellow :: CardDef 'AssetType
theKingInYellow = (weakness "03011" ("The King in Yellow" <:> "Act 1"))
  { cdCardTraits = singleton Tome
  , cdUnique = True
  , cdSlots = [HandSlot]
  }

spiritSpeaker :: CardDef 'AssetType
spiritSpeaker =
  (asset "03014" ("Spirit-Speaker" <:> "Envoy of the Alusi") 2 Neutral)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = singleton Ritual
    }

thirtyTwoColt :: CardDef 'AssetType
thirtyTwoColt = (asset "03020" ".32 Colt" 3 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdUses = Uses Ammo 6
  , cdSlots = [HandSlot]
  }

trueGrit :: CardDef 'AssetType
trueGrit = (asset "03021" "True Grit" 3 Guardian)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Talent
  }

fieldwork :: CardDef 'AssetType
fieldwork = (asset "03024" "Fieldwork" 2 Seeker)
  { cdSkills = [#agility]
  , cdCardTraits = singleton Talent
  }

archaicGlyphs :: CardDef 'AssetType
archaicGlyphs = (asset "03025" ("Archaic Glyphs" <:> "Untranslated") 0 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Occult, Tome]
  , cdSlots = [HandSlot]
  , cdUses = Uses Secret 0
  }

inTheKnow1 :: CardDef 'AssetType
inTheKnow1 = (asset "03027" "In the Know" 3 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = singleton Talent
  , cdUses = Uses Secret 3
  , cdLevel = 1
  }

stealth :: CardDef 'AssetType
stealth = (asset "03028" "Stealth" 2 Rogue)
  { cdSkills = [#agility]
  , cdCardTraits = singleton Talent
  }

lockpicks1 :: CardDef 'AssetType
lockpicks1 = (asset "03031" "Lockpicks" 3 Rogue)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tool, Illicit]
  , cdUses = Uses Supply 3
  , cdLevel = 1
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01687"]
  }

alchemicalTransmutation :: CardDef 'AssetType
alchemicalTransmutation = (asset "03032" "Alchemical Transmutation" 1 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Spell
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  }

spiritAthame1 :: CardDef 'AssetType
spiritAthame1 = (asset "03035" "Spirit Athame" 3 Mystic)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
  , cdLevel = 1
  , cdSlots = [HandSlot]
  }

lantern :: CardDef 'AssetType
lantern = (asset "03036" "Lantern" 2 Survivor)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tool]
  , cdSlots = [HandSlot]
  }

gravediggersShovel :: CardDef 'AssetType
gravediggersShovel = (asset "03037" "Gravedigger's Shovel" 2 Survivor)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
  , cdSlots = [HandSlot]
  }

constanceDumaine :: CardDef 'AssetType
constanceDumaine =
  (storyAsset "03076" ("Constance Dumaine" <:> "Sociable Hostess") 0 TheLastKing
    )
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdDoubleSided = True
    , cdCost = Nothing
    }

jordanPerry :: CardDef 'AssetType
jordanPerry =
  (storyAsset "03077" ("Jordan Perry" <:> "Dignified Financier") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdDoubleSided = True
    , cdCost = Nothing
    }

ishimaruHaruko :: CardDef 'AssetType
ishimaruHaruko =
  (storyAsset "03078" ("Ishimaru Haruko" <:> "Costume Designer") 0 TheLastKing)
    { cdCardTraits = singleton Bystander
    , cdUnique = True
    , cdDoubleSided = True
    , cdCost = Nothing
    }

sebastienMoreau :: CardDef 'AssetType
sebastienMoreau = (storyAsset
                    "03079"
                    ("Sebastien Moreau" <:> "Impassioned Producer")
                    0
                    TheLastKing
                  )
  { cdCardTraits = singleton Bystander
  , cdUnique = True
  , cdDoubleSided = True
  , cdCost = Nothing
  }

ashleighClarke :: CardDef 'AssetType
ashleighClarke = (storyAsset
                   "03080"
                   ("Ashleigh Clarke" <:> "Talented Entertainer")
                   0
                   TheLastKing
                 )
  { cdCardTraits = singleton Bystander
  , cdUnique = True
  , cdDoubleSided = True
  , cdCost = Nothing
  }

combatTraining1 :: CardDef 'AssetType
combatTraining1 = (asset "03107" "Combat Training" 1 Guardian)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Talent, Composure]
  , cdLimits = [LimitPerTrait Composure 1]
  , cdLevel = 1
  }

scientificTheory1 :: CardDef 'AssetType
scientificTheory1 = (asset "03109" "Scientific Theory" 1 Seeker)
  { cdSkills = [#intellect, #combat]
  , cdCardTraits = setFromList [Talent, Composure]
  , cdLimits = [LimitPerTrait Composure 1]
  , cdLevel = 1
  }

knuckleduster :: CardDef 'AssetType
knuckleduster = (asset "03110" "Knuckleduster" 2 Rogue)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
  , cdSlots = [HandSlot]
  }

moxie1 :: CardDef 'AssetType
moxie1 = (asset "03111" "Moxie" 1 Rogue)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Talent, Composure]
  , cdLimits = [LimitPerTrait Composure 1]
  , cdLevel = 1
  }

davidRenfield :: CardDef 'AssetType
davidRenfield =
  (asset "03112" ("David Renfield" <:> "Esteemed Eschatologist") 2 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

grounded1 :: CardDef 'AssetType
grounded1 = (asset "03113" "Grounded" 1 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Talent, Composure]
  , cdLimits = [LimitPerTrait Composure 1]
  , cdLevel = 1
  }

cherishedKeepsake :: CardDef 'AssetType
cherishedKeepsake = (asset "03114" "Cherished Keepsake" 0 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Charm]
  , cdSlots = [AccessorySlot]
  }

plucky1 :: CardDef 'AssetType
plucky1 = (asset "03115" "Plucky" 1 Survivor)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Talent, Composure]
  , cdLimits = [LimitPerTrait Composure 1]
  , cdLevel = 1
  }

mrPeabody :: CardDef 'AssetType
mrPeabody = (storyAsset
              "03141"
              ("Mr. Peabody" <:> "Historical Society Curator")
              0
              EchoesOfThePast
            )
  { cdCardTraits = setFromList [Ally, HistoricalSociety]
  , cdCost = Nothing
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

claspOfBlackOnyx :: CardDef 'AssetType
claspOfBlackOnyx = (storyWeakness
                     "03142"
                     ("Clasp of Black Onyx" <:> "A Gift Unlooked For")
                     EchoesOfThePast
                   )
  { cdCardTraits = setFromList [Item, Relic]
  , cdCost = Just (StaticCost 1)
  , cdRevelation = False
  , cdCardInHandEffects = True
  }

theTatteredCloak :: CardDef 'AssetType
theTatteredCloak = (storyAsset
                     "03143"
                     ("The Tattered Cloak" <:> "Regalia Dementia")
                     2
                     EchoesOfThePast
                   )
  { cdSkills = [#willpower, #combat, #agility]
  , cdCardTraits = setFromList [Item, Clothing]
  , cdSlots = [BodySlot]
  }

trenchKnife :: CardDef 'AssetType
trenchKnife = (asset "03147" "Trench Knife" 1 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot]
  }

charlesRossEsq :: CardDef 'AssetType
charlesRossEsq = (asset
                   "03149"
                   ("Charles Ross, Esq." <:> "Acquisitions and Solicitation")
                   2
                   Seeker
                 )
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Ally, Patron]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

darioElAmin :: CardDef 'AssetType
darioElAmin =
  (asset "03151" ("Dario El-Amin" <:> "Unscrupulous Investor") 4 Rogue)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

bookOfShadows1 :: CardDef 'AssetType
bookOfShadows1 = (asset "03154" "Book of Shadows" 3 Mystic)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tome]
  , cdLevel = 1
  , cdSlots = [HandSlot]
  }

danielChesterfield :: CardDef 'AssetType
danielChesterfield = (storyAsset
                       "03182a"
                       ("Daniel Chesterfield" <:> "He's Not Doing All Too Well"
                       )
                       0
                       TheUnspeakableOath
                     )
  { cdCardTraits = setFromList [Ally, Lunatic]
  , cdCost = Nothing
  , cdUnique = True
  , cdDoubleSided = True
  }

straitjacket :: CardDef 'AssetType
straitjacket = (storyAsset "x03185" "Straitjacket" 0 TheUnspeakableOath)
  { cdCardTraits = setFromList [Item, Clothing]
  , cdSlots = [BodySlot, HandSlot, HandSlot]
  , cdEncounterSetQuantity = Just 2
  , cdCost = Nothing
  , cdClassSymbols = singleton Mythos
  }

fortyFiveAutomatic2 :: CardDef 'AssetType
fortyFiveAutomatic2 = (asset "03190" ".45 Automatic" 4 Guardian)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdSlots = [HandSlot]
  , cdUses = Uses Ammo 4
  , cdLevel = 2
  }

archaicGlyphsGuidingStones3 :: CardDef 'AssetType
archaicGlyphsGuidingStones3 =
  (asset "03192" ("Archaic Glyphs" <:> "Guiding Stones") 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [ArcaneSlot]
    , cdUses = Uses Charge 3
    , cdLevel = 3
    }

archaicGlyphsProphecyForetold3 :: CardDef 'AssetType
archaicGlyphsProphecyForetold3 =
  (asset "03193" ("Archaic Glyphs" <:> "Prophecy Foretold") 2 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Spell
    , cdSlots = [ArcaneSlot]
    , cdUses = Uses Charge 3
    , cdLevel = 3
    }

pickpocketing2 :: CardDef 'AssetType
pickpocketing2 = fast $ (asset "03195" "Pickpocketing" 2 Rogue)
  { cdSkills = [#agility, #agility]
  , cdCardTraits = setFromList [Talent, Illicit]
  , cdLevel = 2
  }

madameLabranche :: CardDef 'AssetType
madameLabranche =
  (asset "03198" ("Madame Labranche" <:> "Mysterious Benefactress") 2 Survivor)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

firstAid3 :: CardDef 'AssetType
firstAid3 = (asset "03230" "First Aid" 2 Guardian)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = setFromList [Talent, Science]
  , cdUses = Uses Supply 4
  , cdLevel = 3
  , cdAlternateCardCodes = ["01683"]
  }

fortyOneDerringer2 :: CardDef 'AssetType
fortyOneDerringer2 = (asset "03234" ".41 Derringer" 3 Rogue)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdUses = Uses Ammo 3
  , cdSlots = [HandSlot]
  , cdLevel = 2
  , cdAlternateCardCodes = ["01688"]
  }

scrying3 :: CardDef 'AssetType
scrying3 = (asset "03236" "Scrying" 1 Mystic)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = setFromList [Spell]
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  , cdLevel = 3
  , cdAlternateCardCodes = ["01690"]
  }

stickToThePlan3 :: CardDef 'AssetType
stickToThePlan3 = permanent $ (asset "03264" "Stick to the Plan" 0 Guardian)
  { cdCardTraits = singleton Talent
  , cdKeywords = setFromList [Keyword.Permanent, Keyword.Exceptional]
  , cdLevel = 3
  }

arcaneInsight4 :: CardDef 'AssetType
arcaneInsight4 = (asset "03266" "Arcane Insight" 3 Seeker)
  { cdCardTraits = singleton Spell
  , cdSkills = [#willpower, #intellect]
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  , cdLevel = 4
  }

suggestion4 :: CardDef 'AssetType
suggestion4 = (asset "03268" "Suggestion" 3 Rogue)
  { cdCardTraits = singleton Spell
  , cdSkills = [#willpower, #agility]
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  , cdLevel = 4
  }

stHubertsKey :: CardDef 'AssetType
stHubertsKey =
  (asset "03269" ("St. Hubert's Key" <:> "Cleansing Fire") 4 Mystic)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSkills = [#willpower]
    , cdSlots = [AccessorySlot]
    , cdUnique = True
    }

arcaneInitiate3 :: CardDef 'AssetType
arcaneInitiate3 = (asset "03271" "Arcane Initiate" 0 Mystic)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Ally, Sorcerer]
  , cdSlots = [AllySlot]
  , cdLevel = 3
  }

armorOfArdennes5 :: CardDef 'AssetType
armorOfArdennes5 = (asset "03305" "Armor of Ardennes" 4 Guardian)
  { cdSkills = [#willpower, #willpower, #combat, #combat]
  , cdCardTraits = setFromList [Item, Armor, Relic]
  , cdSlots = [BodySlot]
  , cdLevel = 5
  }

charonsObol1 :: CardDef 'AssetType
charonsObol1 =
  permanent $ (asset "03308" ("Charon's Obol" <:> "The Ferryman's Pay") 0 Rogue)
    { cdCardTraits = setFromList [Item, Relic]
    , cdLevel = 1
    , cdKeywords = setFromList [Keyword.Permanent, Keyword.Exceptional]
    }

lupara3 :: CardDef 'AssetType
lupara3 = (asset "03309" "Lupara" 3 Rogue)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdLevel = 3
  , cdUses = Uses Ammo 2
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdSlots = [HandSlot]
  }

newspaper2 :: CardDef 'AssetType
newspaper2 = (asset "03313" "Newspaper" 1 Survivor)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Item
  , cdLevel = 2
  , cdSlots = [HandSlot]
  }

keyOfYs :: CardDef 'AssetType
keyOfYs = (asset "03315" ("Key of Ys" <:> "Let the Storm Rage") 3 Neutral)
  { cdSkills = [#wild, #willpower]
  , cdCardTraits = setFromList [Item, Relic]
  , cdLevel = 5
  , cdSlots = [AccessorySlot]
  , cdUnique = True
  }

thePallidMask :: CardDef 'AssetType
thePallidMask =
  (asset "03321b" ("The Pallid Mask" <:> "Chasing Tails") 0 Neutral)
    { cdCardTraits = setFromList [Item, Relic]
    , cdRevelation = True
    }

mitchBrown :: CardDef 'AssetType
mitchBrown = (asset "04006" ("Mitch Brown" <:> "Sole Survivor") 3 Neutral)
  { cdSkills = [#wild, #wild]
  , cdCardTraits = setFromList [Ally, Wayfarer]
  , cdSlots = [AllySlot]
  , cdUnique = True
  }

jakeWilliams :: CardDef 'AssetType
jakeWilliams = (asset "04008" ("Jake Williams" <:> "Loyal Companion") 3 Neutral
               )
  { cdSkills = [#intellect, #wild]
  , cdCardTraits = setFromList [Ally, Wayfarer]
  , cdSlots = [AllySlot]
  , cdUnique = True
  }

finnsTrustyThirtyEight :: CardDef 'AssetType
finnsTrustyThirtyEight = fast $ (asset
                                  "04011"
                                  ("Finn's Trusty .38"
                                  <:> "Never Leave Home Without It"
                                  )
                                  2
                                  Neutral
                                )
  { cdSkills = [#agility, #wild]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdSlots = [HandSlot]
  , cdUses = Uses Ammo 3
  , cdUnique = True
  }

theCodexOfAges :: CardDef 'AssetType
theCodexOfAges =
  (asset "04013" ("The Codex of Ages" <:> "finis omnium nunc est") 2 Neutral)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome, Blessed]
    , cdSlots = [HandSlot]
    , cdKeywords = singleton (Keyword.Seal $ TokenFaceIs Token.ElderSign)
    , cdUnique = True
    }

untilTheEndOfTime :: CardDef 'AssetType
untilTheEndOfTime = (asset "04015" "Until the End of Time" 1 Neutral)
  { cdSkills = [#combat, #wild]
  , cdCardTraits = singleton Talent
  }

survivalKnife :: CardDef 'AssetType
survivalKnife = (asset "04017" "Survival Knife" 2 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot]
  }

venturer :: CardDef 'AssetType
venturer = (asset "04018" "Venturer" 4 Guardian)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Ally, Wayfarer]
  , cdSlots = [AllySlot]
  , cdUses = Uses Supply 3
  }

drElliHorowitz :: CardDef 'AssetType
drElliHorowitz =
  (asset "04021" ("Dr. Elli Horowitz" <:> "Assistant Curator") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Assistant]
    , cdSlots = [AllySlot]
    , cdUnique = True
    }

ancientStone1 :: CardDef 'AssetType
ancientStone1 = (asset "04022" ("Ancient Stone" <:> "Unidentified") 1 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Relic]
  , cdSlots = [HandSlot]
  }

toothOfEztli :: CardDef 'AssetType
toothOfEztli = (asset "04023" ("Tooth of Eztli" <:> "Mortal Reminder") 3 Seeker
               )
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Relic]
  , cdSlots = [AccessorySlot]
  }

treasureHunter1 :: CardDef 'AssetType
treasureHunter1 = (asset "04025" "Treasure Hunter" 1 Rogue)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Ally, Wayfarer]
  , cdSlots = [AllySlot]
  , cdLevel = 1
  }

decoratedSkull :: CardDef 'AssetType
decoratedSkull =
  (asset "04026" ("Decorated Skull" <:> "Doom Begets Doom") 0 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [AccessorySlot]
    , cdUses = Uses Charge 0
    }

mistsOfRlyeh :: CardDef 'AssetType
mistsOfRlyeh = (asset "04029" "Mists of R'lyeh" 2 Mystic)
  { cdSkills = [#agility]
  , cdCardTraits = singleton Spell
  , cdSlots = [ArcaneSlot]
  , cdUses = Uses Charge 4
  }

theChthonianStone :: CardDef 'AssetType
theChthonianStone =
  (asset "04030" ("The Chthonian Stone" <:> "Stygian Waymark") 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [HandSlot]
    , cdUnique = True
    , cdKeywords = singleton
      (Keyword.Seal $ TokenMatchesAny $ map
        TokenFaceIs
        [Token.Skull, Token.Cultist, Token.Tablet, Token.ElderThing]
      )
    }

protectiveIncantation1 :: CardDef 'AssetType
protectiveIncantation1 = (asset "04031" "Protective Incantation" 1 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Ritual, Blessed]
  , cdSlots = [ArcaneSlot]
  , cdKeywords = singleton (Keyword.Seal $ TokenFaceIsNot Token.AutoFail)
  }

yaotl1 :: CardDef 'AssetType
yaotl1 = (asset "04035" ("Yaotl" <:> "Lost Son of Eztli") 3 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Ally, Wayfarer]
  , cdSlots = [AllySlot]
  }

backpack :: CardDef 'AssetType
backpack = (asset "04037" "Backpack" 2 Neutral)
  { cdSkills = [#agility]
  , cdCardTraits = singleton Item
  , cdSlots = [BodySlot]
  }

alejandroVela :: CardDef 'AssetType
alejandroVela = (storyAsset
                  "04051"
                  ("Alejandro Vela" <:> "Renowned Historian")
                  2
                  TheUntamedWilds
                )
  { cdSkills = [#willpower, #intellect, #wild]
  , cdCardTraits = setFromList [Ally, Wayfarer]
  , cdSlots = [AllySlot]
  , cdUnique = True
  }

relicOfAgesADeviceOfSomeSort :: CardDef 'AssetType
relicOfAgesADeviceOfSomeSort = (storyAsset
                                 "04061"
                                 ("Relic of Ages"
                                 <:> "...A Device, of Some Sort"
                                 )
                                 2
                                 TheDoomOfEztli
                               )
  { cdSkills = [#wild, #wild, #wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdUnique = True
  }

shrewdAnalysis :: CardDef 'AssetType
shrewdAnalysis = permanent $ (asset "04106" "Shrewd Analysis" 0 Seeker)
  { cdCardTraits = singleton Talent
  }

luckyCigaretteCase :: CardDef 'AssetType
luckyCigaretteCase = (asset "04107" "Lucky Cigarette Case" 2 Rogue)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Charm]
  , cdSlots = [AccessorySlot]
  , cdAlternateCardCodes = ["60308"]
  }

fence1 :: CardDef 'AssetType
fence1 = (asset "04108" "Fence" 3 Rogue)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Connection, Illicit]
  , cdLevel = 1
  }

arcaneResearch :: CardDef 'AssetType
arcaneResearch = permanent $ (asset "04109" "Arcane Research" 0 Mystic)
  { cdCardTraits = singleton Talent
  , cdPurchaseMentalTrauma = Just 1
  }

harlanEarnstone :: CardDef 'AssetType
harlanEarnstone = (storyAsset
                    "04118b"
                    ("Harlan Earnstone" <:> "Historical Theorist")
                    0
                    ThreadsOfFate
                  )
  { cdCardTraits = setFromList [Bystander, Miskatonic]
  , cdCost = Nothing
  , cdUnique = True
  }

henryDeveau :: CardDef 'AssetType
henryDeveau =
  (storyAsset
      "04125d"
      ("Henry Deveau" <:> "Friend of Alejandro")
      0
      ThreadsOfFate
    )
    { cdCardTraits = singleton Bystander
    , cdCost = Nothing
    , cdUnique = True
    }

mariaDeSilva :: CardDef 'AssetType
mariaDeSilva =
  (storyAsset "04134f" ("Maria DeSilver" <:> "Wealthy Patron") 0 ThreadsOfFate)
    { cdCardTraits = singleton Bystander
    , cdCost = Nothing
    , cdUnique = True
    }

ichtacaTheForgottenGuardian :: CardDef 'AssetType
ichtacaTheForgottenGuardian =
  (storyAsset "04147" ("Ichtaca" <:> "The Forgotten Guardian") 4 ThreadsOfFate)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = setFromList [Ally, Eztli, Wayfarer]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

expeditionJournal :: CardDef 'AssetType
expeditionJournal = (storyAsset "04148" "Expedition Journal" 2 ThreadsOfFate)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = setFromList [Item, Tome]
  , cdUnique = True
  }

wellPrepared2 :: CardDef 'AssetType
wellPrepared2 = (asset "04151" "Well Prepared" 2 Guardian)
  { cdCardTraits = singleton Talent
  , cdLevel = 2
  }

quickStudy2 :: CardDef 'AssetType
quickStudy2 = (asset "04154" "Quick Study" 2 Seeker)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = singleton Talent
  , cdLevel = 2
  }

highRoller2 :: CardDef 'AssetType
highRoller2 = (asset "04156" "High Roller" 2 Rogue)
  { cdSkills = [#intellect, #combat]
  , cdCardTraits = singleton Talent
  , cdLevel = 2
  }

recallTheFuture2 :: CardDef 'AssetType
recallTheFuture2 = (asset "04158" "Recall the Future" 2 Mystic)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Augury, Ritual]
  , cdLevel = 2
  }

tryAndTryAgain1 :: CardDef 'AssetType
tryAndTryAgain1 = (asset "04159" "Try and Try Again" 2 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Talent
  , cdUses = Uses Try 3
  , cdLevel = 1
  }

cornered2 :: CardDef 'AssetType
cornered2 = (asset "04160" "Cornered" 2 Survivor)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = singleton Talent
  , cdLevel = 2
  }

relicOfAgesForestallingTheFuture :: CardDef 'AssetType
relicOfAgesForestallingTheFuture = (storyAsset
                                     "04191"
                                     ("Relic of Ages"
                                     <:> "Forestalling the Future"
                                     )
                                     2
                                     TheBoundaryBeyond
                                   )
  { cdSkills = [#wild, #wild, #wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdUnique = True
  }

otherwordlyCompass2 :: CardDef 'AssetType
otherwordlyCompass2 = (asset "04194" "Otherwordly Compass" 2 Seeker)
  { cdCardTraits = setFromList [Item, Relic]
  , cdSkills = [#intellect, #intellect]
  , cdSlots = [HandSlot]
  , cdLevel = 2
  }

lolaSantiago3 :: CardDef 'AssetType
lolaSantiago3 =
  (asset "04196" ("Lola Santiago" <:> "No-Nonsense Archaeologist") 3 Rogue)
    { cdCardTraits = setFromList [Ally, Wayfarer]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [AllySlot]
    , cdLevel = 3
    }

oliveMcBride :: CardDef 'AssetType
oliveMcBride =
  (asset "04197" ("Olive McBride" <:> "Will Try Anything Once") 2 Mystic)
    { cdCardTraits = setFromList [Ally, Witch]
    , cdSkills = [#willpower]
    , cdSlots = [AllySlot]
    }

trenchCoat :: CardDef 'AssetType
trenchCoat = (asset "04203" "Trench Coat" 3 Neutral)
  { cdCardTraits = setFromList [Item, Clothing]
  , cdSkills = [#agility]
  , cdSlots = [BodySlot]
  }

ornateBow3 :: CardDef 'AssetType
ornateBow3 = (asset "04204" "Ornate Bow" 4 Neutral)
  { cdCardTraits = setFromList [Item, Relic, Weapon, Ranged]
  , cdSkills = [#combat, #agility]
  , cdSlots = [HandSlot, HandSlot]
  , cdUses = Uses Ammo 1
  }

m1918Bar4 :: CardDef 'AssetType
m1918Bar4 = (asset "04229" "M1918 BAR" 5 Guardian)
  { cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdSkills = [#combat, #combat]
  , cdSlots = [HandSlot, HandSlot]
  , cdUses = Uses Ammo 8
  , cdLevel = 4
  }

ancientStoneKnowledgeOfTheElders4 :: CardDef 'AssetType
ancientStoneKnowledgeOfTheElders4 =
  (asset "04230" ("Ancient Stone" <:> "Knowledge of the Elders") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#intellect, #intellect]
    , cdSlots = [HandSlot]
    , cdUses = Uses Secret 0
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = 4
    }

ancientStoneMindsInHarmony4 :: CardDef 'AssetType
ancientStoneMindsInHarmony4 =
  (asset "04231" ("Ancient Stone" <:> "Minds in Harmony") 2 Seeker)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#willpower, #willpower]
    , cdSlots = [HandSlot]
    , cdUses = Uses Secret 0
    , cdKeywords = setFromList [Keyword.Researched YouHaveIdentifiedTheStone]
    , cdLevel = 4
    }

crystallineElderSign3 :: CardDef 'AssetType
crystallineElderSign3 = (asset "04235" "Crystalline Elder Sign" 3 Mystic)
  { cdCardTraits = setFromList [Item, Relic, Blessed]
  , cdSkills = [#wild]
  , cdSlots = [AccessorySlot]
  , cdKeywords = singleton
    (Keyword.Seal $ TokenMatchesAny $ map
      TokenFaceIs
      [Token.PlusOne, Token.ElderSign]
    )
  , cdLevel = 3
  }

onYourOwn3 :: CardDef 'AssetType
onYourOwn3 = (asset "04236" "On Your Own" 2 Survivor)
  { cdCardTraits = singleton Talent
  , cdSkills = [#willpower]
  , cdLimits = [LimitPerInvestigator 1]
  , cdLevel = 3
  }

theCustodian :: CardDef 'AssetType
theCustodian =
  (storyAsset
      "04256"
      ("The Custodian" <:> "Curious Yithian")
      0
      TheCityOfArchives
    )
    { cdCardTraits = setFromList [Ally, Yithian]
    }

handcuffs :: CardDef 'AssetType
handcuffs = (asset "04265" "Handcuffs" 2 Guardian)
  { cdCardTraits = setFromList [Item, Police]
  , cdSkills = [#agility]
  }

feedTheMind3 :: CardDef 'AssetType
feedTheMind3 = (asset "04267" "Feed the Mind" 2 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = singleton Spell
  , cdUses = Uses Secret 3
  , cdSlots = [ArcaneSlot]
  }

coltVestPocket :: CardDef 'AssetType
coltVestPocket = (asset "04268" "Colt Vest Pocket" 2 Rogue)
  { cdSkills = [#agility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdUses = Uses Ammo 5
  , cdSlots = [HandSlot]
  }

theSkeletonKey2 :: CardDef 'AssetType
theSkeletonKey2 = fast $ (asset "04270" "The Skeleton Key" 3 Rogue)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = setFromList [Item, Relic, Cursed]
  , cdUnique = True
  , cdKeywords = setFromList [Keyword.Exceptional]
  }

mistsOfRlyeh4 :: CardDef 'AssetType
mistsOfRlyeh4 = (asset "04271" "Mists of R'lyeh" 2 Mystic)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = singleton Spell
  , cdSlots = [ArcaneSlot]
  , cdUses = Uses Charge 5
  , cdLevel = 4
  }

oldHuntingRifle3 :: CardDef 'AssetType
oldHuntingRifle3 = (asset "04273" "Old Hunting Rifle" 3 Survivor)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdSlots = [HandSlot, HandSlot]
  , cdUses = Uses Ammo 3
  , cdLevel = 3
  }

thermos :: CardDef 'AssetType
thermos = (asset "04274" "Thermos" 4 Neutral)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Item
  , cdUses = Uses Supply 3
  }

hemisphericMap2 :: CardDef 'AssetType
hemisphericMap2 = (asset "04275" "Hemispheric Map" 2 Neutral)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Item, Relic]
  , cdSlots = [AccessorySlot]
  , cdLevel = 2
  }

timewornBrand5 :: CardDef 'AssetType
timewornBrand5 = (asset "04276" "Timeworn Brand" 5 Neutral)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
  , cdSlots = [HandSlot]
  , cdLevel = 5
  }

relicOfAgesRepossessThePast :: CardDef 'AssetType
relicOfAgesRepossessThePast = (storyAsset "04303" ("Relic of Ages" <:> "Repossess the Past") 2 TheDepthsOfYoth)
  { cdSkills = [#wild, #wild, #wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdUnique = True
  }

kerosene1 :: CardDef 'AssetType
kerosene1 = (asset "04304" "Kerosene" 3 Guardian)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Item
  , cdUses = Uses Supply 3
  , cdLevel = 1
  }

flamethrower5 :: CardDef 'AssetType
flamethrower5 = (asset "04305" "Flamethrower" 4 Guardian)
  { cdSkills = [#combat, #combat, #wild]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdUses = Uses Ammo 4
  , cdSlots = [BodySlot, HandSlot, HandSlot]
  , cdLevel = 5
  }

pnakoticManuscripts5 :: CardDef 'AssetType
pnakoticManuscripts5 = (asset "04307" ("Pnakotic Manuscripts" <:> "Mind-Expanding Ideas") 5 Seeker)
  { cdSkills = [#intellect, #wild]
  , cdCardTraits = setFromList [Item, Relic, Tome]
  , cdUses = Uses Secret 3
  , cdSlots = [HandSlot]
  , cdLevel = 5
  }

borrowedTime3 :: CardDef 'AssetType
borrowedTime3 = (asset "04308" "Borrowed Time" 1 Rogue)
  { cdSkills = [#willpower, #agility]
  , cdKeywords = singleton Keyword.Exceptional
  , cdCardTraits = singleton Ritual
  , cdSlots = [ArcaneSlot]
  , cdLevel = 3
  }

shardsOfTheVoid3 :: CardDef 'AssetType
shardsOfTheVoid3 = (asset "04310" "Shard of the Void" 3 Mystic)
  { cdSkills = [#willpower, #combat]
  , cdKeywords = singleton $ Keyword.Seal $ TokenFaceIs Token.Zero
  , cdCardTraits = singleton Spell
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  , cdLevel = 3
  }

sealOfTheSeventhSign5 :: CardDef 'AssetType
sealOfTheSeventhSign5 = (asset "04311" ("Seal of the Seventh Sign" <:> "Over the Threshold and Beyond") 4 Mystic)
  { cdSkills = [#willpower, #wild]
  , cdKeywords = singleton $ Keyword.Seal $ TokenFaceIs Token.AutoFail
  , cdCardTraits = setFromList [Spell, Ritual]
  , cdUses = Uses Charge 7
  , cdSlots = [ArcaneSlot]
  , cdLevel = 5
  }

relicOfAgesUnleashTheTimestream :: CardDef 'AssetType
relicOfAgesUnleashTheTimestream = (storyAsset "04343" ("Relic of Ages" <:> "Unleash the Timestream") 2 ShatteredAeons)
  { cdSkills = [#wild, #wild, #wild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdUnique = True
  }

hypnoticTherapy :: CardDef 'AssetType
hypnoticTherapy = (asset "05007" "Hypnotic Therapy" 2 Neutral)
  { cdCardTraits = singleton Talent
  , cdSkills = [#willpower, #intellect, #wild]
  }

detectivesColt1911s :: CardDef 'AssetType
detectivesColt1911s = (asset "05009" "Detective's Colt 1911s" 4 Neutral)
  { cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdSkills = [#intellect, #combat, #wild]
  , cdSlots = [HandSlot, HandSlot]
  , cdUses = Uses Ammo 4
  }

familyInheritance :: CardDef 'AssetType
familyInheritance = permanent (asset "05011" ("Family Inheritance" <:> "A Windfall? Or a Burden?") 0 Neutral)
  { cdCardTraits = singleton Boon
  }

twilightBlade :: CardDef 'AssetType
twilightBlade = (asset "05013" ("Twilight Blade" <:> "Sanctum's Reward") 3 Neutral)
  { cdCardTraits = setFromList [Item, Relic, Weapon]
  , cdSkills = [#willpower, #combat, #wild]
  , cdSlots = [HandSlot]
  }

baronSamedi :: CardDef 'AssetType
baronSamedi =
  (weakness "05019" ("Baron Samedi" <:> "Lord of the Cemetery"))
    { cdCardTraits = singleton Avatar
    , cdSlots = [AllySlot]
    }

aceOfSwords1 :: CardDef 'AssetType
aceOfSwords1 = (asset "05023" ("Ace of Swords" <:> "Let Your Arrow Fly True") 3 Guardian)
  { cdCardTraits = singleton Tarot
  , cdSlots = [TarotSlot]
  , cdLevel = 1
  , cdCardInHandEffects = True
  }

fingerprintKit :: CardDef 'AssetType
fingerprintKit = (asset "05024" "Fingerprint Kit" 4 Seeker)
  { cdCardTraits = setFromList [Item, Tool]
  , cdSkills = [#intellect]
  , cdSlots = [HandSlot]
  , cdUses = Uses Supply 3
  }

deathXiii1 :: CardDef 'AssetType
deathXiii1 = (asset "05027" ("Death • XIII" <:> "Free from the Past") 3 Seeker)
  { cdCardTraits = singleton Tarot
  , cdSlots = [TarotSlot]
  , cdLevel = 1
  , cdCardInHandEffects = True
  }

wellConnected :: CardDef 'AssetType
wellConnected = (asset "05028" "Well Connected" 2 Rogue)
  { cdCardTraits = singleton Condition
  , cdSkills = [#intellect]
  , cdLimits = [LimitPerInvestigator 1]
  }

theMoonXiii1 :: CardDef 'AssetType
theMoonXiii1 = (asset "05031" ("The Moon • XVIII" <:> "Message from Your Inner Self") 3 Rogue)
  { cdCardTraits = singleton Tarot
  , cdSlots = [TarotSlot]
  , cdLevel = 1
  , cdCardInHandEffects = True
  }

fourOfCups1 :: CardDef 'AssetType
fourOfCups1 = (asset "05035" ("Four of Cups" <:> "Chalice of the Heart") 3 Mystic)
  { cdCardTraits = singleton Tarot
  , cdSlots = [TarotSlot]
  , cdLevel = 1
  , cdCardInHandEffects = True
  }

trackShoes :: CardDef 'AssetType
trackShoes = (asset "05036" "Track Shoes" 3 Survivor)
  { cdCardTraits = setFromList [Item, Clothing, Footwear]
  , cdSkills = [#agility]
  , cdLimits = [LimitPerTrait Footwear 1]
  }

fiveOfPentacles1 :: CardDef 'AssetType
fiveOfPentacles1 = (asset "05039" ("Five of Pentacles" <:> "From the Brink") 3 Survivor)
  { cdCardTraits = singleton Tarot
  , cdSlots = [TarotSlot]
  , cdLevel = 1
  , cdCardInHandEffects = True
  }

aceOfRods1 :: CardDef 'AssetType
aceOfRods1 = (asset "05040" ("Ace of Rods" <:> "The Fateful Step") 3 Neutral)
  { cdCardTraits = singleton Tarot
  , cdSlots = [TarotSlot]
  , cdLevel = 1
  , cdCardInHandEffects = True
  }

theTowerXVI :: CardDef 'AssetType
theTowerXVI =
  (weakness "05042" ("The Tower • XVI" <:> "Circumstances Beyond Your Control"))
    { cdCardTraits = setFromList [Omen, Tarot]
    , cdSlots = [TarotSlot]
    , cdCardInHandEffects = True
    , cdCanReplace = False
    }

meatCleaver :: CardDef 'AssetType
meatCleaver = (asset "05114" "Meat Cleaver" 3 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSlots = [HandSlot]
  }

drawingThin :: CardDef 'AssetType
drawingThin = (asset "05159" "Drawing Thin" 0 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Talent
  }

studious3 :: CardDef 'AssetType
studious3 = permanent $ (asset "05276" "Studious" 0 Survivor)
  { cdCardTraits = singleton Talent
  }

occultLexicon :: CardDef 'AssetType
occultLexicon = (asset "05316" "Occult Lexicon" 2 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tome, Occult]
  , cdSlots = [HandSlot]
  }

scrollOfProphecies :: CardDef 'AssetType
scrollOfProphecies = (asset "06116" "Scroll of Prophecies" 3 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Tome]
  , cdUses = Uses Secret 4
  , cdSlots = [HandSlot]
  }

oldBookOfLore3 :: CardDef 'AssetType
oldBookOfLore3 = (asset "06279" "Old Book of Lore" 2 Seeker)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Item, Tome]
  , cdSlots = [HandSlot]
  , cdAlternateCardCodes = ["01531"]
  , cdUses = Uses Secret 2
  , cdLevel = 3
  }

keenEye :: CardDef 'AssetType
keenEye = (asset "07152" "Keen Eye" 2 Guardian)
  { cdCardTraits = setFromList [Talent]
  , cdSkills = [#intellect, #combat]
  }

ancestralKnowledge3 :: CardDef 'AssetType
ancestralKnowledge3 = permanent $ (asset "07303" "Ancestral Knowledge" 0 Seeker)
  { cdCardTraits = singleton Talent
  , cdKeywords = setFromList [Keyword.Exceptional]
  , cdLevel = 3
  }

livreDeibon :: CardDef 'AssetType
livreDeibon =
  (asset "08005" ("Livre d'Eibon" <:> "Hyperborean Grimoire") 2 Neutral)
    { cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSkills = [#willpower, #willpower, #wild]
    , cdUnique = True
    , cdSlots = [HandSlot]
    }

runicAxe :: CardDef 'AssetType
runicAxe = (asset "09022" "Runix Axe" 4 Guardian)
  { cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSkills = [#combat]
  , cdSlots = [HandSlot, HandSlot]
  }

guardDog2 :: CardDef 'AssetType
guardDog2 = (asset "09034" "Guard Dog" 3 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Ally, Creature]
  , cdSlots = [AllySlot]
  , cdLevel = 2
  }

handcuffs2 :: CardDef 'AssetType
handcuffs2 = fast $ (asset "09035" "Handcuffs" 1 Guardian)
  { cdCardTraits = setFromList [Item, Police]
  , cdSkills = [#combat, #agility]
  , cdLevel = 2
  }

fingerprintKit4 :: CardDef 'AssetType
fingerprintKit4 = (asset "09057" "Fingerprint Kit" 5 Seeker)
  { cdCardTraits = setFromList [Item, Tool]
  , cdSkills = [#intellect, #intellect]
  , cdSlots = [HandSlot]
  , cdUses = Uses Supply 3
  }

physicalTraining2 :: CardDef 'AssetType
physicalTraining2 = (asset "50001" "Physical Training" 0 Guardian)
  { cdSkills = [#willpower, #willpower, #combat, #combat]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

hyperawareness2 :: CardDef 'AssetType
hyperawareness2 = (asset "50003" "Hyperawareness" 0 Seeker)
  { cdSkills = [#intellect, #intellect, #agility, #agility]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

hardKnocks2 :: CardDef 'AssetType
hardKnocks2 = (asset "50005" "Hard Knocks" 0 Rogue)
  { cdSkills = [#combat, #combat, #agility, #agility]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

arcaneStudies2 :: CardDef 'AssetType
arcaneStudies2 = (asset "50007" "Arcane Studies" 0 Mystic)
  { cdSkills = [#willpower, #willpower, #intellect, #intellect]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

digDeep2 :: CardDef 'AssetType
digDeep2 = (asset "50009" "Dig Deep" 0 Survivor)
  { cdSkills = [#willpower, #willpower, #agility, #agility]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

rabbitsFoot3 :: CardDef 'AssetType
rabbitsFoot3 = (asset "50010" "Rabbit's Foot" 1 Survivor)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Item, Charm]
  , cdLevel = 3
  , cdSlots = [AccessorySlot]
  }

riteOfSeeking2 :: CardDef 'AssetType
riteOfSeeking2 = (asset "51007" "Rite of Seeking" 4 Mystic)
  { cdCardTraits = singleton Spell
  , cdSkills = [#intellect]
  , cdLevel = 2
  , cdUses = Uses Charge 3
  , cdSlots = [ArcaneSlot]
  , cdAlternateCardCodes = ["01689"]
  }

coltVestPocket2 :: CardDef 'AssetType
coltVestPocket2 = (asset "53006" "Colt Vest Pocket" 2 Rogue)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdUses = Uses Ammo 5
  , cdSlots = [HandSlot]
  , cdLevel = 2
  }

mistsOfRlyeh2 :: CardDef 'AssetType
mistsOfRlyeh2 = (asset "53007" "Mists of R'lyeh" 2 Mystic)
  { cdSkills = [#agility]
  , cdCardTraits = singleton Spell
  , cdSlots = [ArcaneSlot]
  , cdUses = Uses Charge 5
  , cdLevel = 2
  }

wellConnected3 :: CardDef 'AssetType
wellConnected3 = (asset "54006" "Well Connected" 2 Rogue)
  { cdCardTraits = singleton Condition
  , cdSkills = [#intellect, #agility]
  , cdLimits = [LimitPerInvestigator 1]
  , cdLevel = 3
  }

randallCho :: CardDef 'AssetType
randallCho = (asset "60102" ("Randall Cho" <:> "Concerned Brother") 2 Guardian)
  { cdSkills = [#willpower, #intellect, #wild]
  , cdCardTraits = setFromList [Ally, Medic]
  , cdUnique = True
  , cdSlots = [AllySlot]
  }

boxingGloves :: CardDef 'AssetType
boxingGloves = (asset "60105" "Boxing Gloves" 3 Guardian)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon]
  , cdSlots = [HandSlot, HandSlot]
  }

fleshWard :: CardDef 'AssetType
fleshWard = (asset "60106" "Flesh Ward" 3 Guardian)
  { cdSkills = [#intellect]
  , cdCardTraits = singleton Ritual
  , cdSlots = [ArcaneSlot]
  , cdUses = Uses Charge 4
  }

greteWagner :: CardDef 'AssetType
greteWagner = (asset "60107" ("Grete Wagner" <:> "The Purifier") 5 Guardian)
  { cdSkills = [#intellect, #combat]
  , cdCardTraits = setFromList [Ally, Hunter]
  , cdSlots = [AllySlot]
  , cdUnique = True
  }

relentless :: CardDef 'AssetType
relentless = (asset "60109" "Relentless" 0 Guardian)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = singleton Talent
  }

safeguard :: CardDef 'AssetType
safeguard = (asset "60110" "Safeguard" 2 Guardian)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Talent
  }

boxingGloves3 :: CardDef 'AssetType
boxingGloves3 = (asset "60127" "Boxing Gloves" 2 Guardian)
  { cdSkills = [#combat, #combat]
  , cdCardTraits = setFromList [Item, Weapon]
  , cdSlots = [HandSlot, HandSlot]
  }

greteWagner3 :: CardDef 'AssetType
greteWagner3 = (asset "60128" ("Grete Wagner" <:> "The Purifier") 5 Guardian)
  { cdSkills = [#combat, #intellect, #wild]
  , cdCardTraits = setFromList [Ally, Hunter]
  , cdSlots = [AllySlot]
  , cdLevel = 3
  , cdUnique = True
  }

physicalTraining4 :: CardDef 'AssetType
physicalTraining4 = (asset "60131" "Physical Training" 2 Guardian)
  { cdSkills = [#willpower, #willpower, #combat, #combat]
  , cdCardTraits = setFromList [Talent]
  , cdUses = Uses Resource 2
  , cdLevel = 4
  }

vaultOfKnowledge :: CardDef 'AssetType
vaultOfKnowledge = (asset "60202" "Vault of Knowledge" 3 Seeker)
  { cdSkills = [#willpower, #agility, #wild]
  , cdCardTraits = singleton Talent
  }

arcaneEnlightenment :: CardDef 'AssetType
arcaneEnlightenment = (asset "60205" "Arcane Enlightenment" 2 Seeker)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = setFromList [Ritual]
  , cdSlots = [ArcaneSlot]
  }

celaenoFragments :: CardDef 'AssetType
celaenoFragments =
  (asset "60206" ("Celaeno Fragments" <:> "Book of Books") 1 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdUnique = True
    , cdSlots = [HandSlot]
    }

discOfItzamna :: CardDef 'AssetType
discOfItzamna =
  (asset "60207" ("Disc of Itzamna" <:> "Protective Amulet") 3 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [AccessorySlot]
    }

encyclopedia :: CardDef 'AssetType
encyclopedia = (asset "60208" "Encyclopedia" 2 Seeker)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Item, Tome]
  , cdUses = Uses Secret 5
  , cdSlots = [HandSlot]
  }

feedTheMind :: CardDef 'AssetType
feedTheMind = (asset "60209" "Feed the Mind" 3 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = singleton Spell
  , cdUses = Uses Secret 3
  , cdSlots = [ArcaneSlot]
  }

forbiddenTome :: CardDef 'AssetType
forbiddenTome = (asset "60210" "Forbidden Tome" 1 Seeker)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Item, Relic, Tome]
  , cdUses = Uses Secret 5
  , cdSlots = [HandSlot]
  }

higherEducation :: CardDef 'AssetType
higherEducation = (asset "60211" "Higher Education" 0 Seeker)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Talent]
  }

whittonGreene :: CardDef 'AssetType
whittonGreene =
  (asset "60213" ("Whitton Greene" <:> "Hunter of Rare Books") 4 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

libraryDocent1 :: CardDef 'AssetType
libraryDocent1 = (asset "60220" "Library Docent" 1 Seeker)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  , cdSlots = [AllySlot]
  , cdLevel = 1
  }

esotericAtlas2 :: CardDef 'AssetType
esotericAtlas2 = (asset "60222" "Esoteric Atlas" 3 Seeker)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Item, Tome]
  , cdSlots = [HandSlot]
  , cdLevel = 2
  , cdUses = Uses Secret 4
  }

whittonGreene2 :: CardDef 'AssetType
whittonGreene2 =
  (asset "60223" ("Whitton Greene" <:> "Hunter of Rare Books") 4 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdUnique = True
    , cdSlots = [AllySlot]
    , cdLevel = 2
    }

forbiddenTomeDarkKnowledge3 :: CardDef 'AssetType
forbiddenTomeDarkKnowledge3 =
  (asset "60229" ("Forbidden Tome" <:> "Dark Knowledge") 1 Seeker)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSlots = [HandSlot]
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheTome
    , cdLevel = 3
    }

forbiddenTomeSecretsRevealed3 :: CardDef 'AssetType
forbiddenTomeSecretsRevealed3 =
  (asset "60230" ("Forbidden Tome" <:> "Secrets Revealed") 1 Seeker)
    { cdSkills = [#intellect, #agility, #wild]
    , cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSlots = [HandSlot]
    , cdKeywords = singleton $ Keyword.Researched YouHaveTranslatedTheTome
    , cdLevel = 3
    }

farsight4 :: CardDef 'AssetType
farsight4 =
  (asset "60231" "Farsight" 2 Seeker)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = singleton Ritual
    , cdSlots = [ArcaneSlot]
    , cdLevel = 4
    }

miskatonicArchaeologyFunding4 :: CardDef 'AssetType
miskatonicArchaeologyFunding4 =
  permanent $ (asset "60232" "Miskatonic Archaeology Funding" 0 Seeker)
    { cdCardTraits = singleton Grant
    , cdLevel = 4
    }

theNecronomiconPetrusDeDaciaTranslation5 :: CardDef 'AssetType
theNecronomiconPetrusDeDaciaTranslation5 =
  (asset "60233" ("The Necronomicon" <:> "Petrus de Dacia Translation") 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome]
    , cdUses = Uses Secret 6
    , cdLevel = 5
    }

lockpicks :: CardDef 'AssetType
lockpicks = (asset "60305" "Lockpicks" 3 Rogue)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tool, Illicit]
  , cdSlots = [HandSlot]
  }

streetwise :: CardDef 'AssetType
streetwise = (asset "60311" "Streetwise" 0 Rogue)
  { cdCardTraits = singleton Talent
  }

eighteenDerringer :: CardDef 'AssetType
eighteenDerringer = (asset "60505" ".18 Derringer" 3 Survivor)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdUses = Uses Ammo 2
  , cdSlots = [HandSlot]
  }

grimmsFairyTales :: CardDef 'AssetType
grimmsFairyTales = (asset "60506" "Grimm's Fairy Tales" 2 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Item, Tome]
  , cdUses = Uses Secret 4
  , cdSlots = [HandSlot]
  }

oldKeyring :: CardDef 'AssetType
oldKeyring = (asset "60507" "Old Keyring" 1 Survivor)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Item, Tool]
  , cdUses = Uses Uses.Key 2
  , cdSlots = [HandSlot]
  }

grannyOrne :: CardDef 'AssetType
grannyOrne = (asset "60508" ("Granny Orne" <:> "Tough Old Bird") 4 Survivor)
  { cdSkills = [#intellect]
  , cdCardTraits = singleton Ally
  , cdSlots = [AllySlot]
  , cdUnique = True
  }

mysteriousRaven :: CardDef 'AssetType
mysteriousRaven = (asset "60509" "Mysterious Raven" 1 Survivor)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Ally, Creature]
  , cdSlots = [AllySlot]
  }

scrapper :: CardDef 'AssetType
scrapper = (asset "60511" "Scrapper" 2 Survivor)
  { cdCardTraits = setFromList [Talent]
  , cdSkills = [#combat, #agility]
  }

cherishedKeepsake1 :: CardDef 'AssetType
cherishedKeepsake1 = (asset "60520" "Cherished Keepsake" 0 Survivor)
  { cdCardTraits = setFromList [Item, Charm]
  , cdSlots = [AccessorySlot]
  , cdSkills = [#willpower]
  , cdLevel = 1
  }

leatherCoat1 :: CardDef 'AssetType
leatherCoat1 = (asset "60521" "Leather Coat" 0 Survivor)
  { cdSkills = [#combat]
  , cdCardTraits = setFromList [Item, Armor]
  , cdSlots = [BodySlot]
  , cdLevel = 1
  }

eighteenDerringer2 :: CardDef 'AssetType
eighteenDerringer2 = (asset "60522" ".18 Derringer" 2 Survivor)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  , cdUses = Uses Ammo 3
  , cdSlots = [HandSlot]
  , cdLevel = 2
  }

grannyOrne3 :: CardDef 'AssetType
grannyOrne3 = (asset "60527" ("Granny Orne" <:> "Tough Old Bird") 4 Survivor)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = singleton Ally
  , cdSlots = [AllySlot]
  , cdUnique = True
  , cdLevel = 3
  }

chainsaw4 :: CardDef 'AssetType
chainsaw4 = (asset "60529" "Chainsaw" 4 Survivor)
  { cdSkills = [#combat, #combat, #combat]
  , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
  , cdUses = Uses Supply 3
  , cdSlots = [HandSlot, HandSlot]
  , cdLevel = 4
  }

quickLearner4 :: CardDef 'AssetType
quickLearner4 = permanent $ (asset "60530" "Quick Learner" 0 Survivor)
  { cdCardTraits = singleton Condition
  , cdLevel = 4
  }

dejaVu5 :: CardDef 'AssetType
dejaVu5 = permanent $ (asset "60531" "Déjà Vu" 0 Survivor)
  { cdCardTraits = setFromList [Talent, Cursed]
  , cdLevel = 5
  }

ladyEsprit :: CardDef 'AssetType
ladyEsprit =
  (storyAsset "81019" ("Lady Esprit" <:> "Dangerous Bokor") 4 TheBayou)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdUnique = True
    , cdSlots = [AllySlot]
    }

bearTrap :: CardDef 'AssetType
bearTrap = (storyAsset "81020" "Bear Trap" 0 TheBayou)
  { cdCardTraits = setFromList [Trap]
  , cdCost = Nothing
  }

fishingNet :: CardDef 'AssetType
fishingNet = (storyAsset "81021" "Fishing Net" 0 TheBayou)
  { cdCardTraits = setFromList [Trap]
  , cdCost = Nothing
  }

monstrousTransformation :: CardDef 'AssetType
monstrousTransformation =
  fast $ (storyAsset "81030" "Monstrous Transformation" 0 CurseOfTheRougarou)
    { cdCardTraits = setFromList [Talent]
    }

maskedCarnevaleGoer_17 :: CardDef 'AssetType
maskedCarnevaleGoer_17 =
  (storyAsset "82017b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    }

maskedCarnevaleGoer_18 :: CardDef 'AssetType
maskedCarnevaleGoer_18 =
  (storyAsset "82018b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    }

maskedCarnevaleGoer_19 :: CardDef 'AssetType
maskedCarnevaleGoer_19 =
  (storyAsset "82019b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    }

maskedCarnevaleGoer_20 :: CardDef 'AssetType
maskedCarnevaleGoer_20 =
  (storyAsset "82020b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    }

innocentReveler :: CardDef 'AssetType
innocentReveler =
  (storyAssetWithMany "82021" "Innocent Reveler" 0 CarnevaleOfHorrors 3)
    { cdCardTraits = setFromList [Ally, Bystander, Carnevale]
    , cdCost = Nothing
    }

maskedCarnevaleGoer_21 :: CardDef 'AssetType
maskedCarnevaleGoer_21 =
  (storyAsset "82021b" "Masked Carnevale-Goer" 0 CarnevaleOfHorrors)
    { cdCardTraits = singleton Carnevale
    }

abbessAllegriaDiBiase :: CardDef 'AssetType
abbessAllegriaDiBiase = (storyAsset
                          "82022"
                          ("Abbess Allegria Di Biase" <:> "Most Blessed")
                          4
                          CarnevaleOfHorrors
                        )
  { cdCardTraits = setFromList [Ally, Believer]
  , cdUnique = True
  , cdSkills = [#willpower, #intellect, #wild]
  , cdSlots = [AllySlot]
  }

bauta :: CardDef 'AssetType
bauta = (storyAsset "82023" "Bauta" 1 CarnevaleOfHorrors)
  { cdCardTraits = setFromList [Item, Mask]
  , cdSkills = [#combat, #wild]
  , cdLimits = [LimitPerTrait Mask 1]
  }

medicoDellaPeste :: CardDef 'AssetType
medicoDellaPeste =
  (storyAsset "82024" "Medico Della Peste" 1 CarnevaleOfHorrors)
    { cdCardTraits = setFromList [Item, Mask]
    , cdSkills = [#willpower, #wild]
    , cdLimits = [LimitPerTrait Mask 1]
    }

pantalone :: CardDef 'AssetType
pantalone = (storyAsset "82025" "Pantalone" 1 CarnevaleOfHorrors)
  { cdCardTraits = setFromList [Item, Mask]
  , cdSkills = [#intellect, #wild]
  , cdLimits = [LimitPerTrait Mask 1]
  }

gildedVolto :: CardDef 'AssetType
gildedVolto = (storyAsset "82026" "Gilded Volto" 1 CarnevaleOfHorrors)
  { cdCardTraits = setFromList [Item, Mask]
  , cdSkills = [#agility, #wild]
  , cdLimits = [LimitPerTrait Mask 1]
  }

daisysToteBagAdvanced :: CardDef 'AssetType
daisysToteBagAdvanced = (asset "90002" "Daisy's Tote Bag" 2 Neutral)
  { cdSkills = [#willpower, #intellect, #wild, #wild]
  , cdCardTraits = setFromList [Item]
  , cdUnique = True
  }

theNecronomiconAdvanced :: CardDef 'AssetType
theNecronomiconAdvanced =
  (weakness "90003" ("The Necronomicon" <:> "John Dee Translation"))
    { cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [HandSlot]
    }

courage :: CardDef 'AssetType
courage =
  (asset "xcourage" "Courage" 0 Neutral) { cdCardTraits = singleton Courage }

intrepid :: CardDef 'AssetType
intrepid =
  (asset "xintrepid" "Intrepid" 0 Guardian) { cdCardTraits = singleton Innate }
