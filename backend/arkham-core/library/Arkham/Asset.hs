{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Id

createAsset :: IsCard a => a -> AssetId -> Asset
createAsset a aId =
  lookupAsset (toCardCode a) aId (toCardOwner a) (toCardId a)

lookupAsset :: CardCode -> AssetId -> Maybe InvestigatorId -> CardId -> Asset
lookupAsset cardCode = case lookup cardCode allAssets of
  Nothing -> error $ "Unknown asset: " <> show cardCode
  Just (SomeAssetCard a) -> \aid mId cId -> Asset $ cbCardBuilder a cId (aid, mId)

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \o -> do
    cCode <- o .: "cardCode"
    withAssetCardCode cCode
      $ \(_ :: AssetCard a) -> Asset <$> parseJSON @a (Object o)

withAssetCardCode
  :: CardCode -> (forall a . IsAsset a => AssetCard a -> r) -> r
withAssetCardCode cCode f = case lookup cCode allAssets of
  Nothing -> error "invalid assets"
  Just (SomeAssetCard a) -> f a

allAssets :: HashMap CardCode SomeAssetCard
allAssets = mapFrom
  someAssetCardCode
  [ -- Night of the Zealot
  --- signature [notz]
    SomeAssetCard rolands38Special
  , SomeAssetCard daisysToteBag
  , SomeAssetCard theNecronomicon
  , SomeAssetCard heirloomOfHyperborea
  , SomeAssetCard wendysAmulet
  --- guardian [notz]
  , SomeAssetCard fortyFiveAutomatic
  , SomeAssetCard physicalTraining
  , SomeAssetCard beatCop
  , SomeAssetCard firstAid
  , SomeAssetCard machete
  , SomeAssetCard guardDog
  , SomeAssetCard policeBadge2
  , SomeAssetCard beatCop2
  , SomeAssetCard shotgun4
  --- seeker [notz]
  , SomeAssetCard magnifyingGlass
  , SomeAssetCard oldBookOfLore
  , SomeAssetCard researchLibrarian
  , SomeAssetCard drMilanChristopher
  , SomeAssetCard hyperawareness
  , SomeAssetCard medicalTexts
  , SomeAssetCard magnifyingGlass1
  , SomeAssetCard discOfItzamna2
  , SomeAssetCard encyclopedia2
  --- rogue [notz]
  , SomeAssetCard switchblade
  , SomeAssetCard burglary
  , SomeAssetCard pickpocketing
  , SomeAssetCard fortyOneDerringer
  , SomeAssetCard leoDeLuca
  , SomeAssetCard hardKnocks
  , SomeAssetCard leoDeLuca1
  , SomeAssetCard catBurglar1
  --- mystic [notz]
  , SomeAssetCard forbiddenKnowledge
  , SomeAssetCard holyRosary
  , SomeAssetCard shrivelling
  , SomeAssetCard scrying
  , SomeAssetCard arcaneStudies
  , SomeAssetCard arcaneInitiate
  , SomeAssetCard bookOfShadows3
  , SomeAssetCard grotesqueStatue4
  --- survivor [notz]
  , SomeAssetCard leatherCoat
  , SomeAssetCard scavenging
  , SomeAssetCard baseballBat
  , SomeAssetCard rabbitsFoot
  , SomeAssetCard strayCat
  , SomeAssetCard digDeep
  , SomeAssetCard aquinnah1
  --- neutral [notz]
  , SomeAssetCard knife
  , SomeAssetCard flashlight
  , SomeAssetCard bulletproofVest3
  , SomeAssetCard elderSignAmulet3
  --- story [notz]
  , SomeAssetCard litaChantler
  -- The Dunwich Legacy
  --- signature [tdl]
  , SomeAssetCard zoeysCross
  , SomeAssetCard jennysTwin45s
  , SomeAssetCard jimsTrumpet
  , SomeAssetCard duke
  --- guardian [tdl]
  , SomeAssetCard blackjack
  --- seeker [tdl]
  , SomeAssetCard laboratoryAssistant
  , SomeAssetCard strangeSolution
  --- rogue [tdl]
  , SomeAssetCard liquidCourage
  , SomeAssetCard hiredMuscle1
  --- mystic [tdl]
  , SomeAssetCard riteOfSeeking
  , SomeAssetCard ritualCandles
  , SomeAssetCard clarityOfMind
  --- survivor [tdl]
  , SomeAssetCard fireAxe
  , SomeAssetCard peterSylvestre
  , SomeAssetCard peterSylvestre2
  --- neutral [tdl]
  , SomeAssetCard kukri
  --- story [tdl]
  , SomeAssetCard drHenryArmitage
  , SomeAssetCard alchemicalConcoction
  , SomeAssetCard jazzMulligan
  , SomeAssetCard professorWarrenRice
  , SomeAssetCard peterClover
  , SomeAssetCard drFrancisMorgan
  -- The Miskatonic Museum
  --- guardian [tmm]
  , SomeAssetCard brotherXavier1
  --- seeker [tmm]
  , SomeAssetCard pathfinder1
  --- rogue [tmm]
  , SomeAssetCard adaptable1
  --- mytic [tmm]
  , SomeAssetCard songOfTheDead2
  --- survivor [tmm]
  , SomeAssetCard fireExtinguisher1
  --- neutral [tmm]
  , SomeAssetCard smokingPipe
  , SomeAssetCard painkillers
  --- story [tmm]
  , SomeAssetCard haroldWalsted
  , SomeAssetCard adamLynch
  , SomeAssetCard theNecronomiconOlausWormiusTranslation
  -- The Essex County Express
  --- guardian [tece]
  , SomeAssetCard bandolier
  --- seeker [tece]
  , SomeAssetCard artStudent
  --- rogue [tece]
  , SomeAssetCard switchblade2
  --- mystic [tece]
  , SomeAssetCard shrivelling3
  --- survivor [tece]
  , SomeAssetCard newspaper
  --- neutral [tece]
  , SomeAssetCard relicHunter3
  , SomeAssetCard charisma3
  --- story [tece]
  , SomeAssetCard helplessPassenger
  -- Blood on the Altar
  --- guardian [bota]
  , SomeAssetCard keenEye3
  --- seeker [bota]
  , SomeAssetCard higherEducation3
  --- rogue [bota]
  , SomeAssetCard loneWolf
  , SomeAssetCard streetwise3
  --- mystic [bota]
  , SomeAssetCard bloodPact3
  --- survivor [bota]
  , SomeAssetCard scrapper3
  --- story [bota]
  , SomeAssetCard keyToTheChamber
  , SomeAssetCard zebulonWhateley
  , SomeAssetCard earlSawyer
  , SomeAssetCard powderOfIbnGhazi
  -- Undimensioned and Unseen
  --- guardian [uau]
  , SomeAssetCard springfieldM19034
  --- rogue [uau]
  , SomeAssetCard luckyDice2
  --- mystic [uau]
  , SomeAssetCard alyssaGraham
  , SomeAssetCard riteOfSeeking4
  --- survivor [uau]
  , SomeAssetCard darkHorse
  --- story [uau]
  , SomeAssetCard esotericFormula
  -- Where Doom Awaits
  --- seeker [wda]
  , SomeAssetCard strangeSolutionRestorativeConcoction4
  , SomeAssetCard strangeSolutionAcidicIchor4
  , SomeAssetCard strangeSolutionFreezingVariant4
  --- rogue [wda]
  , SomeAssetCard joeyTheRatVigil
  --- mystic [wda]
  , SomeAssetCard jewelOfAureolus3
  --- neutral [wda]
  , SomeAssetCard fineClothes
  -- Lost in Time and Space
  --- guardian [litas]
  , SomeAssetCard lightningGun5
  --- seeker [litas]
  , SomeAssetCard drWilliamTMaleson
  --- rogue [litas]
  , SomeAssetCard chicagoTypewriter4
  , SomeAssetCard theGoldPocketWatch4
  --- mystic [litas]
  , SomeAssetCard shrivelling5
  --- survivor [litas]
  , SomeAssetCard aquinnah3
  , SomeAssetCard tryAndTryAgain3
  --- neutral [litas]
  , SomeAssetCard theRedGlovedMan5
  -- The Path to Carcosa
  --- signature [ptc]
  , SomeAssetCard sophieInLovingMemory
  , SomeAssetCard sophieItWasAllMyFault
  , SomeAssetCard analyticalMind
  , SomeAssetCard theKingInYellow
  , SomeAssetCard spiritSpeaker
  --- guardian [ptc]
  , SomeAssetCard thirtyTwoColt
  , SomeAssetCard trueGrit
  --- seeker [ptc]
  , SomeAssetCard fieldwork
  , SomeAssetCard archaicGlyphs
  , SomeAssetCard inTheKnow1
  --- rogue [ptc]
  , SomeAssetCard stealth
  , SomeAssetCard lockpicks1
  --- mystic [ptc]
  , SomeAssetCard alchemicalTransmutation
  , SomeAssetCard spiritAthame1
  --- survivor [ptc]
  , SomeAssetCard lantern
  , SomeAssetCard gravediggersShovel
  --- story [ptc]
  , SomeAssetCard constanceDumaine
  , SomeAssetCard jordanPerry
  , SomeAssetCard ishimaruHaruko
  , SomeAssetCard sebastienMoreau
  , SomeAssetCard ashleighClarke
  -- Echoes of the Past
  --- guardian [eotp]
  , SomeAssetCard combatTraining1
  --- seeker [eotp]
  , SomeAssetCard scientificTheory1
  --- rogue [eotp]
  , SomeAssetCard knuckleduster
  , SomeAssetCard moxie1
  --- mystic [eotp]
  , SomeAssetCard davidRenfield
  , SomeAssetCard grounded1
  --- survivor [eotp]
  , SomeAssetCard cherishedKeepsake
  , SomeAssetCard plucky1
  --- story [eotp]
  , SomeAssetCard mrPeabody
  , SomeAssetCard claspOfBlackOnyx
  , SomeAssetCard theTatteredCloak
  -- The Unspeakable Oath
  --- guardian [tuo]
  , SomeAssetCard trenchKnife
  --- seeker [tuo]
  , SomeAssetCard charlesRossEsq
  --- rogue [tuo]
  , SomeAssetCard darioElAmin
  --- mystic [tuo]
  , SomeAssetCard bookOfShadows1
  --- story [tuo]
  , SomeAssetCard danielChesterfield
  , SomeAssetCard straitjacket
  -- A Phantom of Truth
  --- guardian [apot]
  , SomeAssetCard fortyFiveAutomatic2
  --- seeker [apot]
  , SomeAssetCard archaicGlyphsGuidingStones3
  , SomeAssetCard archaicGlyphsProphecyForetold3
  --- rogue [apot]
  , SomeAssetCard pickpocketing2
  --- survivor [apot]
  , SomeAssetCard madameLabranche
  -- The Pallid Mask
  --- guardian [tpm]
  , SomeAssetCard firstAid3
  --- rogue [tpm]
  , SomeAssetCard fortyOneDerringer2
  --- mystic [tpm]
  , SomeAssetCard scrying3
  -- Black Stars Rise
  --- guardian [bsr]
  , SomeAssetCard stickToThePlan3
  --- seeker [bsr]
  , SomeAssetCard arcaneInsight4
  --- rogue [bsr]
  , SomeAssetCard suggestion4
  --- mystic [bsr]
  , SomeAssetCard stHubertsKey
  , SomeAssetCard arcaneInitiate3
  -- Dim Carcosa
  --- guardian [dca]
  , SomeAssetCard armorOfArdennes5
  --- rogue [dca]
  , SomeAssetCard charonsObol1
  , SomeAssetCard lupara3
  --- survivor [dca]
  , SomeAssetCard newspaper2
  --- neutal [dca]
  , SomeAssetCard keyOfYs
  --- story [dca]
  , SomeAssetCard thePallidMask
  -- The Forgotten Age
  --- signature [tfa]
  , SomeAssetCard mitchBrown
  , SomeAssetCard jakeWilliams
  , SomeAssetCard finnsTrustyThirtyEight
  , SomeAssetCard theCodexOfAges
  , SomeAssetCard untilTheEndOfTime
  --- guardian [tfa]
  , SomeAssetCard survivalKnife
  , SomeAssetCard venturer
  --- seeker [tfa]
  , SomeAssetCard drElliHorowitz
  , SomeAssetCard ancientStone1
  , SomeAssetCard toothOfEztli
  --- rogue [tfa]
  , SomeAssetCard treasureHunter1
  , SomeAssetCard decoratedSkull
  --- mystic [tfa]
  , SomeAssetCard mistsOfRlyeh
  , SomeAssetCard theChthonianStone
  , SomeAssetCard protectiveIncantation1
  --- survivor [tfa]
  , SomeAssetCard yaotl1
  --- neutral [tfa]
  , SomeAssetCard backpack
  --- story [tfa]
  , SomeAssetCard alejandroVela
  , SomeAssetCard relicOfAgesADeviceOfSomeSort
  -- Thread of Fate
  --- seeker [tof]
  , SomeAssetCard shrewdAnalysis
  --- rogue [tof]
  , SomeAssetCard luckyCigaretteCase
  , SomeAssetCard fence1
  --- mystic [tof]
  , SomeAssetCard arcaneResearch
  --- story [tfa]
  , SomeAssetCard harlanEarnstone
  , SomeAssetCard henryDeveau
  , SomeAssetCard mariaDeSilva
  , SomeAssetCard ichtacaTheForgottenGuardian
  , SomeAssetCard expeditionJournal
  -- The Boundary Beyond
  --- guardian [tbb]
  , SomeAssetCard wellPrepared2
  --- seeker [tbb]
  , SomeAssetCard quickStudy2
  --- rogue [tbb]
  , SomeAssetCard highRoller2
  --- mystic [tbb]
  , SomeAssetCard recallTheFuture2
  --- survivor [tbb]
  , SomeAssetCard tryAndTryAgain1
  , SomeAssetCard cornered2
  --- story [tbb]
  , SomeAssetCard relicOfAgesForestallingTheFuture
  -- Heart of the Elders
  --- guardian [hote]
  , SomeAssetCard intrepid
  --- seeker [hote]
  , SomeAssetCard otherworldlyCompass2
  --- rogue [hote]
  , SomeAssetCard lolaSantiago3
  --- mystic [hote]
  , SomeAssetCard oliveMcBride
  --- neutral [hote]
  , SomeAssetCard trenchCoat
  , SomeAssetCard ornateBow3
  -- The City of Archives
  --- guardian [tcoa]
  , SomeAssetCard m1918Bar4
  --- seeker [tcoa]
  , SomeAssetCard ancientStoneKnowledgeOfTheElders4
  , SomeAssetCard ancientStoneMindsInHarmony4
  --- mystic [tcoa]
  , SomeAssetCard crystallineElderSign3
  --- survivor [tcoa]
  , SomeAssetCard onYourOwn3
  --- story [tcoa]
  , SomeAssetCard theCustodian
  -- The Depths of Yoth
  --- guardian [tdoy]
  , SomeAssetCard handcuffs
  --- seeker [tdoy]
  , SomeAssetCard feedTheMind3
  --- rogue [tdoy]
  , SomeAssetCard coltVestPocket
  , SomeAssetCard theSkeletonKey2
  --- mystic [tdoy]
  , SomeAssetCard mistsOfRlyeh4
  --- survivor [tdoy]
  , SomeAssetCard oldHuntingRifle3
  --- neutral [tdoy]
  , SomeAssetCard thermos
  , SomeAssetCard hemisphericMap3
  , SomeAssetCard timewornBrand5
  --- story [tdoy]
  , SomeAssetCard relicOfAgesRepossessThePast
  --- Shattered Aeons
  --- guardian [sha]
  , SomeAssetCard kerosene1
  , SomeAssetCard flamethrower5
  --- seeker [sha]
  , SomeAssetCard pnakoticManuscripts5
  --- rogue [sha]
  , SomeAssetCard borrowedTime3
  --- mystic [sha]
  , SomeAssetCard shardsOfTheVoid3
  , SomeAssetCard sealOfTheSeventhSign5
  --- story [sha]
  , SomeAssetCard relicOfAgesUnleashTheTimestream
  -- The Circle Undone
  --- signature [tcu]
  , SomeAssetCard hypnoticTherapy
  , SomeAssetCard detectivesColt1911s
  , SomeAssetCard familyInheritance
  , SomeAssetCard twilightBlade
  , SomeAssetCard baronSamedi
  --- guardian [tcu]
  , SomeAssetCard aceOfSwords1
  --- seeker [tcu]
  , SomeAssetCard fingerprintKit
  , SomeAssetCard deathXiii1
  --- rogue [tcu]
  , SomeAssetCard wellConnected
  , SomeAssetCard theMoonXiii1
  --- mystic [tcu]
  , SomeAssetCard fourOfCups1
  --- survivor [tcu]
  , SomeAssetCard trackShoes
  , SomeAssetCard fiveOfPentacles1
  --- neutral [tcu]
  , SomeAssetCard aceOfRods1
  --- weakness [tcu]
  , SomeAssetCard theTowerXVI
  -- The Secret Name
  --- guardian [tsn]
  , SomeAssetCard somethingWorthFightingFor
  --- mystic [tsn]
  , SomeAssetCard signMagick
  --- survivor [tsn]
  , SomeAssetCard meatCleaver
  --- multi [tsn]
  , SomeAssetCard fortyFiveThompson
  , SomeAssetCard scrollOfSecrets
  , SomeAssetCard tennesseeSourMash
  , SomeAssetCard enchantedBlade
  , SomeAssetCard grislyTotem
  -- The Wages of Sin
  --- guardian [wos]
  , SomeAssetCard aliceLuxley
  --- seeker [wos]
  , SomeAssetCard mrRook
  , SomeAssetCard hawkEyeFoldingCamera
  --- rogue [wos]
  , SomeAssetCard henryWan
  --- mystic [wos]
  , SomeAssetCard wither
  , SomeAssetCard sixthSense
  --- survivor [wos]
  , SomeAssetCard drawingThin
  -- For the Greater Good
  --- survivor + rogue [fgg]
  , SomeAssetCard fortyFiveThompsonGuardian3
  , SomeAssetCard fortyFiveThompsonRogue3
  --- seeker + mystic [fgg]
  , SomeAssetCard scrollOfSecretsSeeker3
  , SomeAssetCard scrollOfSecretsMystic3
  --- rogue + survivor [fgg]
  , SomeAssetCard tennesseeSourMashRogue3
  , SomeAssetCard tennesseeSourMashSurvivor3
  --- guardian + mystic [fgg]
  , SomeAssetCard enchantedBladeGuardian3
  , SomeAssetCard enchantedBladeMystic3
  --- seeker + survivor [fgg]
  , SomeAssetCard grislyTotemSeeker3
  , SomeAssetCard grislyTotemSurvivor3
  --- neutral [fgg]
  , SomeAssetCard theCouncilsCoffer2
  -- Union and Disillusion
  --- seeker [uad]
  , SomeAssetCard esotericAtlas1
  --- rogue [uad]
  , SomeAssetCard investments
  --- mystic [uad]
  , SomeAssetCard deVermisMysteriis2
  --- survivor [uad]
  , SomeAssetCard guidingSpirit1
  -- In the Clutches of Chaos
  --- guardian [icc]
  , SomeAssetCard mk1Grenades4
  , SomeAssetCard agencyBackup5
  --- seeker [icc]
  , SomeAssetCard studious3
  --- rogue [icc]
  , SomeAssetCard anotherDayAnotherDollar3
  -- Before the Black Throne
  --- guardian [bbt]
  , SomeAssetCard hallowedMirror
  --- seeker [bbt]
  , SomeAssetCard occultLexicon
  --- mystic [bbt]
  , SomeAssetCard wither4
  , SomeAssetCard sixthSense4
  -- The Search for Kadath
  --- mystic [sfk]
  , SomeAssetCard scrollOfProphecies
  -- Where the Gods Dwell
  --- seeker [sfk]
  , SomeAssetCard oldBookOfLore3
  -- The Innsmouth Conspiracy
  --- signature [tic]
  , SomeAssetCard showmanship
  , SomeAssetCard occultScraps
  -- Devil Reef
  --- guardian [def]
  , SomeAssetCard keenEye
  -- Into the Maelstrom
  --- seeker [itm]
  , SomeAssetCard ancestralKnowledge3
  -- Edge of the Earth
  --- signature [eote]
  , SomeAssetCard livreDeibon
  -- The Scarlet Keys
  --- guardian [tsk]
  -- , SomeAssetCard runicAxe
  , SomeAssetCard guardDog2
  , SomeAssetCard handcuffs2
  --- seeker [tsk]
  , SomeAssetCard fingerprintKit4
  --- rogue [tsk]
  , SomeAssetCard chuckFergus2
  -- Return to Night of the Zealot
  --- guardian [rtnotz]
  , SomeAssetCard physicalTraining2
  --- seeker [rtnotz]
  , SomeAssetCard hyperawareness2
  --- rogue [rtnotz]
  , SomeAssetCard hardKnocks2
  --- mystic [rtnotz]
  , SomeAssetCard arcaneStudies2
  --- survivor [rtnotz]
  , SomeAssetCard digDeep2
  , SomeAssetCard rabbitsFoot3
  -- Return to the Dunwich Legacy
  --- guardian [rtdwl]
  , SomeAssetCard bandolier2
  --- mystic [rtdwl]
  , SomeAssetCard riteOfSeeking2
  -- Return to the Forgotten Age
  --- rogue [rttfa]
  , SomeAssetCard coltVestPocket2
  --- mystic [rttfa]
  , SomeAssetCard mistsOfRlyeh2
  --- neutral [rttfa]
  , SomeAssetCard backpack2
  -- Return to the circle undone
  -- rogue [rttcu]
  , SomeAssetCard wellConnected3
  -- mystic [rttcu]
  , SomeAssetCard signMagick3
  -- Investigator Starter Decks
  --- Nathaniel Cho
  , SomeAssetCard randallCho
  , SomeAssetCard boxingGloves
  , SomeAssetCard fleshWard
  , SomeAssetCard greteWagner
  , SomeAssetCard relentless
  , SomeAssetCard safeguard
  , SomeAssetCard boxingGloves3
  , SomeAssetCard greteWagner3
  , SomeAssetCard physicalTraining4
  --- Harvel Walters
  , SomeAssetCard vaultOfKnowledge
  , SomeAssetCard arcaneEnlightenment
  , SomeAssetCard celaenoFragments
  , SomeAssetCard discOfItzamna
  , SomeAssetCard encyclopedia
  , SomeAssetCard feedTheMind
  , SomeAssetCard forbiddenTome
  , SomeAssetCard higherEducation
  , SomeAssetCard whittonGreene
  , SomeAssetCard libraryDocent1
  , SomeAssetCard esotericAtlas2
  , SomeAssetCard whittonGreene2
  , SomeAssetCard forbiddenTomeDarkKnowledge3
  , SomeAssetCard forbiddenTomeSecretsRevealed3
  , SomeAssetCard farsight4
  , SomeAssetCard miskatonicArchaeologyFunding4
  , SomeAssetCard theNecronomiconPetrusDeDaciaTranslation5
  --- Winnifred Habbamock
  , SomeAssetCard lockpicks
  , SomeAssetCard mauserC96
  , SomeAssetCard lonnieRitter
  , SomeAssetCard leatherJacket
  , SomeAssetCard streetwise
  , SomeAssetCard liquidCourage1
  , SomeAssetCard mauserC962
  , SomeAssetCard luckyCigaretteCase3
  , SomeAssetCard sharpshooter3
  , SomeAssetCard berettaM19184
  , SomeAssetCard chuckFergus5
  --- Jacqueline Fine
  , SomeAssetCard arbiterOfFates
  , SomeAssetCard scryingMirror
  , SomeAssetCard azureFlame
  , SomeAssetCard clairvoyance
  , SomeAssetCard ineffableTruth
  , SomeAssetCard familiarSpirit
  , SomeAssetCard crystalPendulum
  , SomeAssetCard robesOfEndlessNight
  , SomeAssetCard grotesqueStatue2
  , SomeAssetCard robesOfEndlessNight2
  , SomeAssetCard azureFlame3
  , SomeAssetCard clairvoyance3
  , SomeAssetCard ineffableTruth3
  , SomeAssetCard arcaneStudies4
  , SomeAssetCard azureFlame5
  , SomeAssetCard clairvoyance5
  , SomeAssetCard ineffableTruth5
  --- Stella Clark
  , SomeAssetCard eighteenDerringer
  , SomeAssetCard grimmsFairyTales
  , SomeAssetCard oldKeyring
  , SomeAssetCard grannyOrne
  , SomeAssetCard mysteriousRaven
  , SomeAssetCard scrapper
  , SomeAssetCard cherishedKeepsake1
  , SomeAssetCard leatherCoat1
  , SomeAssetCard eighteenDerringer2
  , SomeAssetCard grannyOrne3
  , SomeAssetCard chainsaw4
  , SomeAssetCard quickLearner4
  , SomeAssetCard dejaVu5
  -- Curse of the Rougarou
  --- story [cotr]
  , SomeAssetCard ladyEsprit
  , SomeAssetCard bearTrap
  , SomeAssetCard fishingNet
  , SomeAssetCard monstrousTransformation
  -- Carnevale of Horrors
  --- story [coh]
  , SomeAssetCard maskedCarnevaleGoer_17
  , SomeAssetCard maskedCarnevaleGoer_18
  , SomeAssetCard maskedCarnevaleGoer_19
  , SomeAssetCard maskedCarnevaleGoer_20
  , SomeAssetCard innocentReveler
  , SomeAssetCard maskedCarnevaleGoer_21
  , SomeAssetCard abbessAllegriaDiBiase
  , SomeAssetCard bauta
  , SomeAssetCard medicoDellaPeste
  , SomeAssetCard pantalone
  , SomeAssetCard gildedVolto
  -- Parallel Investigators
  --- Read or Die
  , SomeAssetCard daisysToteBagAdvanced
  , SomeAssetCard theNecronomiconAdvanced
  -- Promo
  --- Blood of Baalshandor
  , SomeAssetCard mollyMaxwell
  -- Misc
  , SomeAssetCard courage
  ]
