{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Id

createAsset :: IsCard a => a -> AssetId -> Asset
createAsset a aId =
  let this = lookupAsset (toCardCode a) aId (toCardOwner a) (toCardId a)
   in overAttrs (\attrs -> attrs {assetCustomizations = customizations}) this
 where
  customizations = case toCard a of
    PlayerCard pc -> pcCustomizations pc
    _ -> mempty

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
  :: CardCode -> (forall a. IsAsset a => AssetCard a -> r) -> r
withAssetCardCode cCode f = case lookup cCode allAssets of
  Nothing -> error "invalid assets"
  Just (SomeAssetCard a) -> f a

allAssets :: Map CardCode SomeAssetCard
allAssets =
  mapFrom
    someAssetCardCode
    [ -- Night of the Zealot
      --- signature [notz]
      SomeAssetCard rolands38Special
    , SomeAssetCard daisysToteBag
    , SomeAssetCard theNecronomicon
    , SomeAssetCard heirloomOfHyperborea
    , SomeAssetCard wendysAmulet
    , --- guardian [notz]
      SomeAssetCard fortyFiveAutomatic
    , SomeAssetCard physicalTraining
    , SomeAssetCard beatCop
    , SomeAssetCard firstAid
    , SomeAssetCard machete
    , SomeAssetCard guardDog
    , SomeAssetCard policeBadge2
    , SomeAssetCard beatCop2
    , SomeAssetCard shotgun4
    , --- seeker [notz]
      SomeAssetCard magnifyingGlass
    , SomeAssetCard oldBookOfLore
    , SomeAssetCard researchLibrarian
    , SomeAssetCard drMilanChristopher
    , SomeAssetCard hyperawareness
    , SomeAssetCard medicalTexts
    , SomeAssetCard magnifyingGlass1
    , SomeAssetCard discOfItzamna2
    , SomeAssetCard encyclopedia2
    , --- rogue [notz]
      SomeAssetCard switchblade
    , SomeAssetCard burglary
    , SomeAssetCard pickpocketing
    , SomeAssetCard fortyOneDerringer
    , SomeAssetCard leoDeLuca
    , SomeAssetCard hardKnocks
    , SomeAssetCard leoDeLuca1
    , SomeAssetCard catBurglar1
    , --- mystic [notz]
      SomeAssetCard forbiddenKnowledge
    , SomeAssetCard holyRosary
    , SomeAssetCard shrivelling
    , SomeAssetCard scrying
    , SomeAssetCard arcaneStudies
    , SomeAssetCard arcaneInitiate
    , SomeAssetCard bookOfShadows3
    , SomeAssetCard grotesqueStatue4
    , --- survivor [notz]
      SomeAssetCard leatherCoat
    , SomeAssetCard scavenging
    , SomeAssetCard baseballBat
    , SomeAssetCard rabbitsFoot
    , SomeAssetCard strayCat
    , SomeAssetCard digDeep
    , SomeAssetCard aquinnah1
    , --- neutral [notz]
      SomeAssetCard knife
    , SomeAssetCard flashlight
    , SomeAssetCard bulletproofVest3
    , SomeAssetCard elderSignAmulet3
    , --- story [notz]
      SomeAssetCard litaChantler
    , -- The Dunwich Legacy
      --- signature [tdl]
      SomeAssetCard zoeysCross
    , SomeAssetCard jennysTwin45s
    , SomeAssetCard jimsTrumpet
    , SomeAssetCard duke
    , --- guardian [tdl]
      SomeAssetCard blackjack
    , --- seeker [tdl]
      SomeAssetCard laboratoryAssistant
    , SomeAssetCard strangeSolution
    , --- rogue [tdl]
      SomeAssetCard liquidCourage
    , SomeAssetCard hiredMuscle1
    , --- mystic [tdl]
      SomeAssetCard riteOfSeeking
    , SomeAssetCard ritualCandles
    , SomeAssetCard clarityOfMind
    , --- survivor [tdl]
      SomeAssetCard fireAxe
    , SomeAssetCard peterSylvestre
    , SomeAssetCard peterSylvestre2
    , --- neutral [tdl]
      SomeAssetCard kukri
    , --- story [tdl]
      SomeAssetCard drHenryArmitage
    , SomeAssetCard alchemicalConcoction
    , SomeAssetCard jazzMulligan
    , SomeAssetCard professorWarrenRice
    , SomeAssetCard peterClover
    , SomeAssetCard drFrancisMorgan
    , -- The Miskatonic Museum
      --- guardian [tmm]
      SomeAssetCard brotherXavier1
    , --- seeker [tmm]
      SomeAssetCard pathfinder1
    , --- rogue [tmm]
      SomeAssetCard adaptable1
    , --- mytic [tmm]
      SomeAssetCard songOfTheDead2
    , --- survivor [tmm]
      SomeAssetCard fireExtinguisher1
    , --- neutral [tmm]
      SomeAssetCard smokingPipe
    , SomeAssetCard painkillers
    , --- story [tmm]
      SomeAssetCard haroldWalsted
    , SomeAssetCard adamLynch
    , SomeAssetCard theNecronomiconOlausWormiusTranslation
    , -- The Essex County Express
      --- guardian [tece]
      SomeAssetCard bandolier
    , --- seeker [tece]
      SomeAssetCard artStudent
    , --- rogue [tece]
      SomeAssetCard switchblade2
    , --- mystic [tece]
      SomeAssetCard shrivelling3
    , --- survivor [tece]
      SomeAssetCard newspaper
    , --- neutral [tece]
      SomeAssetCard relicHunter3
    , SomeAssetCard charisma3
    , --- story [tece]
      SomeAssetCard helplessPassenger
    , -- Blood on the Altar
      --- guardian [bota]
      SomeAssetCard keenEye3
    , --- seeker [bota]
      SomeAssetCard higherEducation3
    , --- rogue [bota]
      SomeAssetCard loneWolf
    , SomeAssetCard streetwise3
    , --- mystic [bota]
      SomeAssetCard bloodPact3
    , --- survivor [bota]
      SomeAssetCard scrapper3
    , --- story [bota]
      SomeAssetCard keyToTheChamber
    , SomeAssetCard zebulonWhateley
    , SomeAssetCard earlSawyer
    , SomeAssetCard powderOfIbnGhazi
    , -- Undimensioned and Unseen
      --- guardian [uau]
      SomeAssetCard springfieldM19034
    , --- rogue [uau]
      SomeAssetCard luckyDice2
    , --- mystic [uau]
      SomeAssetCard alyssaGraham
    , SomeAssetCard riteOfSeeking4
    , --- survivor [uau]
      SomeAssetCard darkHorse
    , --- story [uau]
      SomeAssetCard esotericFormula
    , -- Where Doom Awaits
      --- seeker [wda]
      SomeAssetCard strangeSolutionRestorativeConcoction4
    , SomeAssetCard strangeSolutionAcidicIchor4
    , SomeAssetCard strangeSolutionFreezingVariant4
    , --- rogue [wda]
      SomeAssetCard joeyTheRatVigil
    , --- mystic [wda]
      SomeAssetCard jewelOfAureolus3
    , --- neutral [wda]
      SomeAssetCard fineClothes
    , -- Lost in Time and Space
      --- guardian [litas]
      SomeAssetCard lightningGun5
    , --- seeker [litas]
      SomeAssetCard drWilliamTMaleson
    , --- rogue [litas]
      SomeAssetCard chicagoTypewriter4
    , SomeAssetCard theGoldPocketWatch4
    , --- mystic [litas]
      SomeAssetCard shrivelling5
    , --- survivor [litas]
      SomeAssetCard aquinnah3
    , SomeAssetCard tryAndTryAgain3
    , --- neutral [litas]
      SomeAssetCard theRedGlovedMan5
    , -- The Path to Carcosa
      --- signature [ptc]
      SomeAssetCard sophieInLovingMemory
    , SomeAssetCard sophieItWasAllMyFault
    , SomeAssetCard analyticalMind
    , SomeAssetCard theKingInYellow
    , SomeAssetCard spiritSpeaker
    , --- guardian [ptc]
      SomeAssetCard thirtyTwoColt
    , SomeAssetCard trueGrit
    , --- seeker [ptc]
      SomeAssetCard fieldwork
    , SomeAssetCard archaicGlyphs
    , SomeAssetCard inTheKnow1
    , --- rogue [ptc]
      SomeAssetCard stealth
    , SomeAssetCard lockpicks1
    , --- mystic [ptc]
      SomeAssetCard alchemicalTransmutation
    , SomeAssetCard spiritAthame1
    , --- survivor [ptc]
      SomeAssetCard lantern
    , SomeAssetCard gravediggersShovel
    , --- story [ptc]
      SomeAssetCard constanceDumaine
    , SomeAssetCard jordanPerry
    , SomeAssetCard ishimaruHaruko
    , SomeAssetCard sebastienMoreau
    , SomeAssetCard ashleighClarke
    , -- Echoes of the Past
      --- guardian [eotp]
      SomeAssetCard combatTraining1
    , --- seeker [eotp]
      SomeAssetCard scientificTheory1
    , --- rogue [eotp]
      SomeAssetCard knuckleduster
    , SomeAssetCard moxie1
    , --- mystic [eotp]
      SomeAssetCard davidRenfield
    , SomeAssetCard grounded1
    , --- survivor [eotp]
      SomeAssetCard cherishedKeepsake
    , SomeAssetCard plucky1
    , --- story [eotp]
      SomeAssetCard mrPeabody
    , SomeAssetCard claspOfBlackOnyx
    , SomeAssetCard theTatteredCloak
    , -- The Unspeakable Oath
      --- guardian [tuo]
      SomeAssetCard trenchKnife
    , --- seeker [tuo]
      SomeAssetCard charlesRossEsq
    , --- rogue [tuo]
      SomeAssetCard darioElAmin
    , --- mystic [tuo]
      SomeAssetCard bookOfShadows1
    , --- story [tuo]
      SomeAssetCard danielChesterfield
    , SomeAssetCard straitjacket
    , -- A Phantom of Truth
      --- guardian [apot]
      SomeAssetCard fortyFiveAutomatic2
    , --- seeker [apot]
      SomeAssetCard archaicGlyphsGuidingStones3
    , SomeAssetCard archaicGlyphsProphecyForetold3
    , --- rogue [apot]
      SomeAssetCard pickpocketing2
    , --- survivor [apot]
      SomeAssetCard madameLabranche
    , -- The Pallid Mask
      --- guardian [tpm]
      SomeAssetCard firstAid3
    , --- rogue [tpm]
      SomeAssetCard fortyOneDerringer2
    , --- mystic [tpm]
      SomeAssetCard scrying3
    , -- Black Stars Rise
      --- guardian [bsr]
      SomeAssetCard stickToThePlan3
    , --- seeker [bsr]
      SomeAssetCard arcaneInsight4
    , --- rogue [bsr]
      SomeAssetCard suggestion4
    , --- mystic [bsr]
      SomeAssetCard stHubertsKey
    , SomeAssetCard arcaneInitiate3
    , -- Dim Carcosa
      --- guardian [dca]
      SomeAssetCard armorOfArdennes5
    , --- rogue [dca]
      SomeAssetCard charonsObol1
    , SomeAssetCard lupara3
    , --- survivor [dca]
      SomeAssetCard newspaper2
    , --- neutal [dca]
      SomeAssetCard keyOfYs
    , --- story [dca]
      SomeAssetCard thePallidMask
    , -- The Forgotten Age
      --- signature [tfa]
      SomeAssetCard mitchBrown
    , SomeAssetCard jakeWilliams
    , SomeAssetCard finnsTrustyThirtyEight
    , SomeAssetCard theCodexOfAges
    , SomeAssetCard untilTheEndOfTime
    , --- guardian [tfa]
      SomeAssetCard survivalKnife
    , SomeAssetCard venturer
    , --- seeker [tfa]
      SomeAssetCard drElliHorowitz
    , SomeAssetCard ancientStone1
    , SomeAssetCard toothOfEztli
    , --- rogue [tfa]
      SomeAssetCard treasureHunter1
    , SomeAssetCard decoratedSkull
    , --- mystic [tfa]
      SomeAssetCard mistsOfRlyeh
    , SomeAssetCard theChthonianStone
    , SomeAssetCard protectiveIncantation1
    , --- survivor [tfa]
      SomeAssetCard yaotl1
    , --- neutral [tfa]
      SomeAssetCard backpack
    , --- story [tfa]
      SomeAssetCard alejandroVela
    , SomeAssetCard relicOfAgesADeviceOfSomeSort
    , -- Thread of Fate
      --- seeker [tof]
      SomeAssetCard shrewdAnalysis
    , --- rogue [tof]
      SomeAssetCard luckyCigaretteCase
    , SomeAssetCard fence1
    , --- mystic [tof]
      SomeAssetCard arcaneResearch
    , --- story [tfa]
      SomeAssetCard harlanEarnstone
    , SomeAssetCard henryDeveau
    , SomeAssetCard mariaDeSilva
    , SomeAssetCard ichtacaTheForgottenGuardian
    , SomeAssetCard expeditionJournal
    , -- The Boundary Beyond
      --- guardian [tbb]
      SomeAssetCard wellPrepared2
    , --- seeker [tbb]
      SomeAssetCard quickStudy2
    , --- rogue [tbb]
      SomeAssetCard highRoller2
    , --- mystic [tbb]
      SomeAssetCard recallTheFuture2
    , --- survivor [tbb]
      SomeAssetCard tryAndTryAgain1
    , SomeAssetCard cornered2
    , --- story [tbb]
      SomeAssetCard relicOfAgesForestallingTheFuture
    , -- Heart of the Elders
      --- guardian [hote]
      SomeAssetCard intrepid
    , --- seeker [hote]
      SomeAssetCard otherworldlyCompass2
    , --- rogue [hote]
      SomeAssetCard lolaSantiago3
    , --- mystic [hote]
      SomeAssetCard oliveMcBride
    , --- neutral [hote]
      SomeAssetCard trenchCoat
    , SomeAssetCard ornateBow3
    , -- The City of Archives
      --- guardian [tcoa]
      SomeAssetCard m1918Bar4
    , --- seeker [tcoa]
      SomeAssetCard ancientStoneKnowledgeOfTheElders4
    , SomeAssetCard ancientStoneMindsInHarmony4
    , --- mystic [tcoa]
      SomeAssetCard crystallineElderSign3
    , --- survivor [tcoa]
      SomeAssetCard onYourOwn3
    , --- story [tcoa]
      SomeAssetCard theCustodian
    , -- The Depths of Yoth
      --- guardian [tdoy]
      SomeAssetCard handcuffs
    , --- seeker [tdoy]
      SomeAssetCard feedTheMind3
    , --- rogue [tdoy]
      SomeAssetCard coltVestPocket
    , SomeAssetCard theSkeletonKey2
    , --- mystic [tdoy]
      SomeAssetCard mistsOfRlyeh4
    , --- survivor [tdoy]
      SomeAssetCard oldHuntingRifle3
    , --- neutral [tdoy]
      SomeAssetCard thermos
    , SomeAssetCard hemisphericMap3
    , SomeAssetCard timewornBrand5
    , --- story [tdoy]
      SomeAssetCard relicOfAgesRepossessThePast
    , --- Shattered Aeons
      --- guardian [sha]
      SomeAssetCard kerosene1
    , SomeAssetCard flamethrower5
    , --- seeker [sha]
      SomeAssetCard pnakoticManuscripts5
    , --- rogue [sha]
      SomeAssetCard borrowedTime3
    , --- mystic [sha]
      SomeAssetCard shardsOfTheVoid3
    , SomeAssetCard sealOfTheSeventhSign5
    , --- story [sha]
      SomeAssetCard relicOfAgesUnleashTheTimestream
    , -- The Circle Undone
      --- signature [tcu]
      SomeAssetCard hypnoticTherapy
    , SomeAssetCard detectivesColt1911s
    , SomeAssetCard familyInheritance
    , SomeAssetCard twilightBlade
    , SomeAssetCard baronSamedi
    , --- guardian [tcu]
      SomeAssetCard aceOfSwords1
    , --- seeker [tcu]
      SomeAssetCard fingerprintKit
    , SomeAssetCard deathXiii1
    , --- rogue [tcu]
      SomeAssetCard wellConnected
    , SomeAssetCard theMoonXiii1
    , --- mystic [tcu]
      SomeAssetCard fourOfCups1
    , --- survivor [tcu]
      SomeAssetCard trackShoes
    , SomeAssetCard fiveOfPentacles1
    , --- neutral [tcu]
      SomeAssetCard aceOfRods1
    , --- weakness [tcu]
      SomeAssetCard theTowerXVI
    , -- The Secret Name
      --- guardian [tsn]
      SomeAssetCard somethingWorthFightingFor
    , --- mystic [tsn]
      SomeAssetCard signMagick
    , --- survivor [tsn]
      SomeAssetCard meatCleaver
    , --- multi [tsn]
      SomeAssetCard fortyFiveThompson
    , SomeAssetCard scrollOfSecrets
    , SomeAssetCard tennesseeSourMash
    , SomeAssetCard enchantedBlade
    , SomeAssetCard grislyTotem
    , --- story [tfa]
      SomeAssetCard theBlackBook
    , -- The Wages of Sin
      --- guardian [wos]
      SomeAssetCard aliceLuxley
    , --- seeker [wos]
      SomeAssetCard mrRook
    , SomeAssetCard hawkEyeFoldingCamera
    , --- rogue [wos]
      SomeAssetCard henryWan
    , --- mystic [wos]
      SomeAssetCard wither
    , SomeAssetCard sixthSense
    , --- survivor [wos]
      SomeAssetCard drawingThin
    , --- story
      SomeAssetCard spectralWeb
    , -- For the Greater Good
      --- survivor + rogue [fgg]
      SomeAssetCard fortyFiveThompsonGuardian3
    , SomeAssetCard fortyFiveThompsonRogue3
    , --- seeker + mystic [fgg]
      SomeAssetCard scrollOfSecretsSeeker3
    , SomeAssetCard scrollOfSecretsMystic3
    , --- rogue + survivor [fgg]
      SomeAssetCard tennesseeSourMashRogue3
    , SomeAssetCard tennesseeSourMashSurvivor3
    , --- guardian + mystic [fgg]
      SomeAssetCard enchantedBladeGuardian3
    , SomeAssetCard enchantedBladeMystic3
    , --- seeker + survivor [fgg]
      SomeAssetCard grislyTotemSeeker3
    , SomeAssetCard grislyTotemSurvivor3
    , --- neutral [fgg]
      SomeAssetCard theCouncilsCoffer2
    , --- story [fgg]
      SomeAssetCard augustLindquist
    , SomeAssetCard puzzleBox
    , -- Union and Disillusion
      --- seeker [uad]
      SomeAssetCard esotericAtlas1
    , --- rogue [uad]
      SomeAssetCard investments
    , --- mystic [uad]
      SomeAssetCard deVermisMysteriis2
    , --- survivor [uad]
      SomeAssetCard guidingSpirit1
    , --- story [uad]
      SomeAssetCard gavriellaMizrah
    , SomeAssetCard jeromeDavids
    , SomeAssetCard pennyWhite
    , SomeAssetCard valentinoRivas
    , -- In the Clutches of Chaos
      --- guardian [icc]
      SomeAssetCard mk1Grenades4
    , SomeAssetCard agencyBackup5
    , --- seeker [icc]
      SomeAssetCard studious3
    , --- rogue [icc]
      SomeAssetCard anotherDayAnotherDollar3
    , --- mystic [icc]
      SomeAssetCard dayanaEsperence3
    , --- neutral [icc]
      SomeAssetCard annaKaslow4
    , -- Before the Black Throne
      --- guardian [bbt]
      SomeAssetCard hallowedMirror
    , --- seeker [bbt]
      SomeAssetCard occultLexicon
    , --- rogue [bbt]
      SomeAssetCard doubleDouble4
    , --- mystic [bbt]
      SomeAssetCard wither4
    , SomeAssetCard sixthSense4
    , -- The Dream-Eaters
      --- signature [tde]
      SomeAssetCard becky
    , SomeAssetCard bountyContracts
    , SomeAssetCard tonys38LongColt
    , SomeAssetCard gateBox
    , SomeAssetCard patricesViolin
    , --- guardian [tde]
      SomeAssetCard theHungeringBlade1
    , SomeAssetCard solemnVow
    , --- seeker [tde]
      SomeAssetCard segmentOfOnyx1
    , SomeAssetCard pendantOfTheQueen
    , --- rogue [tde]
      SomeAssetCard crystallizerOfDreams
    , --- survivor [tde]
      SomeAssetCard missDoyle1
    , SomeAssetCard hope
    , SomeAssetCard zeal
    , SomeAssetCard augur
    , --- basic weakness [tde]
      SomeAssetCard kleptomania
    , -- Beyond the Gates of Sleep
      --- story [tde]
      SomeAssetCard randolphCarterExpertDreamer
    , -- Waking Nightmare
      --- story [tde]
      SomeAssetCard randolphCarterChainedToTheWakingWorld
    , SomeAssetCard drShivaniMaheswaran
    , -- The Search for Kadath
      --- seeker [sfk]
      SomeAssetCard dreamDiary
    , --- mystic [sfk]
      SomeAssetCard scrollOfProphecies
    , --- survivor [sfk]
      SomeAssetCard jessicaHyde1
    , --- story [tde]
      SomeAssetCard virgilGray
    , -- A Thousand Shapes of Horror
      --- guardian [tsh]
      SomeAssetCard tetsuoMori
    , --- seeker [tsh]
      SomeAssetCard otherworldCodex2
    , SomeAssetCard dreamEnhancingSerum
    , --- rogue [tsh]
      SomeAssetCard gregoryGry
    , --- mystic [tsh]
      SomeAssetCard healingWords
    , --- neutral [tsh]
      SomeAssetCard versatile2
    , --- story [tsh]
      SomeAssetCard theSilverKey
    , -- The Dark Side of the Moon
      --- guardian [dsm]
      SomeAssetCard thirtyFiveWinchester
    , SomeAssetCard safeguard2
    , --- rogue [dsm]
      SomeAssetCard burglary2
    , --- survivor [dsm]
      SomeAssetCard moonstone
    , --- story [dsm]
      SomeAssetCard virgilGrayTrulyInspired
    , SomeAssetCard theCaptain
    , -- Point of No Return
      --- seeker [pnr]
      SomeAssetCard dreamDiaryDreamsOfAnExplorer3
    , SomeAssetCard dreamDiaryDreamsOfAMadman3
    , SomeAssetCard dreamDiaryDreamsOfAChild3
    , --- rogue [pnr]
      SomeAssetCard haste2
    , --- mystic [pnr]
      SomeAssetCard empowerSelfStamina2
    , SomeAssetCard empowerSelfAlacrity2
    , SomeAssetCard empowerSelfAcuity2
    , SomeAssetCard twilaKatherinePrice3
    , --- story [pnr]
      SomeAssetCard richardUptonPickman
    , -- Where the Gods Dwell
      --- guardian [wgd]
      SomeAssetCard emptyVessel4
    , SomeAssetCard wishEater
    , --- seeker [wgd]
      SomeAssetCard oldBookOfLore3
    , --- rogue [wgd]
      SomeAssetCard garroteWire2
    , SomeAssetCard delilahORourke3
    , --- mystic [wgd]
      SomeAssetCard summonedHound1
    , --- neutral [wgd]
      SomeAssetCard theBlackCat5
    , -- Weaver of the Cosmos
      --- guardian [woc]
      SomeAssetCard spiritualResolve5
    , --- seeker [woc]
      SomeAssetCard abigailForeman4
    , --- rogue [woc]
      SomeAssetCard joeyTheRatVigil3
    , SomeAssetCard sawedOffShotgun5
    , --- mystic [woc]
      SomeAssetCard mindsEye2
    , SomeAssetCard shiningTrapezohedron4
    , --- survivor [woc]
      SomeAssetCard nightmareBauble3
    , SomeAssetCard scavenging2
    , -- The Innsmouth Conspiracy
      --- signature [tic]
      SomeAssetCard guardianAngel
    , SomeAssetCard showmanship
    , SomeAssetCard occultScraps
    , SomeAssetCard seaChangeHarpoon
    , SomeAssetCard silassNet
    , --- guardian [tic]
      SomeAssetCard bookOfPsalms
    , SomeAssetCard blessedBlade
    , SomeAssetCard riteOfSanctification
    , --- seeker [tic]
      SomeAssetCard cryptographicCipher
    , SomeAssetCard crypticGrimoireUntranslated
    , --- rogue [tic]
      SomeAssetCard twentyFiveAutomatic
    , SomeAssetCard darkRitual
    , SomeAssetCard obfuscation
    , --- mystic [tic]
      SomeAssetCard swordCane
    , --- survivor [tic]
      SomeAssetCard tokenOfFaith
    , -- In Too Deep
      --- guardian [itp]
      SomeAssetCard riotWhistle
    , SomeAssetCard sacredCovenant2
    , --- seeker [itp]
      SomeAssetCard eldritchSophist
    , SomeAssetCard blasphemousCovenant2
    , --- rogue [itp]
      SomeAssetCard falseCovenant2
    , --- mystic [itp]
      SomeAssetCard armageddon
    , SomeAssetCard eyeOfChaos
    , SomeAssetCard shroudOfShadows
    , SomeAssetCard paradoxicalCovenant2
    , --- survivor [itp]
      SomeAssetCard marinersCompass
    , SomeAssetCard ancientCovenant2
    , -- Devil Reef
      --- guardian [def]
      SomeAssetCard keenEye
    , --- rogue [def]
      SomeAssetCard priestOfTwoFaiths1
    , --- mystic [def]
      SomeAssetCard bloodPact
    , SomeAssetCard abyssalTome2
    , -- Horror in High Geat
      --- guardian [hhg]
      SomeAssetCard enchantedArmor2
    , SomeAssetCard blessingOfIsis3
    , --- seeker [hhg]
      SomeAssetCard crypticGrimoireTextOfTheElderHerald4
    , SomeAssetCard crypticGrimoireTextOfTheElderGuardian4
    , --- rogue [hhg]
      SomeAssetCard tristanBotleyFixerForHire2
    , --- mystic [hhg]
      SomeAssetCard curseOfAeons3
    , -- A Light in the Fog
      --- guardian [lif]
      SomeAssetCard holyRosary2
    , SomeAssetCard shieldOfFaith2
    , --- seeker [lif]
      SomeAssetCard guidedByTheUnseen3
    , --- rogue [lif]
      SomeAssetCard luckyPennyOmenOfMisfortune2
    , SomeAssetCard eyeOfTheDjinnVesselOfGoodAndEvil2
    , --- mystic [lif]
      SomeAssetCard armageddon4
    , SomeAssetCard eyeOfChaos4
    , SomeAssetCard shroudOfShadows4
    , --- survivor [lif]
      SomeAssetCard spiritOfHumanity2
    , -- The Lair of Dagon
      --- guardian [lod]
      SomeAssetCard nephthysHuntressOfBast4
    , --- seeker [lod]
      SomeAssetCard hyperawareness4
    , --- rogue [lod]
      SomeAssetCard geas2
    , SomeAssetCard hardKnocks4
    , --- mystic [lod]
      SomeAssetCard ikiaqTheCouncilsChosen3
    , SomeAssetCard fluteOfTheOuterGods4
    , --- survivor [lod]
      SomeAssetCard digDeep4
    , --- neutral [lod]
      SomeAssetCard favorOfTheMoon1
    , SomeAssetCard favorOfTheSun1
    , SomeAssetCard purifyingCorruption4
    , -- Into the Maelstrom
      --- guardian [itm]
      SomeAssetCard holySpear5
    , --- seeker [itm]
      SomeAssetCard ancestralKnowledge3
    , SomeAssetCard ariadnesTwine3
    , --- rogue [itm]
      SomeAssetCard twentyFiveAutomatic2
    , SomeAssetCard luckyDice3
    , --- survivor [itm]
      SomeAssetCard jacobMorrisonCostGuardCaptain3
    , -- Edge of the Earth
      --- signature [eote]
      SomeAssetCard mechanicsWrench
    , SomeAssetCard livreDeibon
    , SomeAssetCard trustyBullwhip
    , SomeAssetCard disciplineAlignmentOfSpirit
    , SomeAssetCard disciplineAlignmentOfSpiritBroken
    , SomeAssetCard disciplineQuiescenceOfThought
    , SomeAssetCard disciplineQuiescenceOfThoughtBroken
    , SomeAssetCard disciplinePrescienceOfFate
    , SomeAssetCard disciplinePrescienceOfFateBroken
    , SomeAssetCard disciplineBalanceOfBody
    , SomeAssetCard disciplineBalanceOfBodyBroken
    , SomeAssetCard shrewdDealings
    , --- guardian [eote]
      SomeAssetCard gearedUp
    , SomeAssetCard butterflySwords2
    , SomeAssetCard combatTraining3
    , SomeAssetCard butterflySwords5
    , --- seeker [eote]
      SomeAssetCard forcedLearning
    , SomeAssetCard jeremiahKirbyArcticArchaeologist
    , SomeAssetCard archiveOfConduitsUnidentified
    , SomeAssetCard hikingBoots1
    , SomeAssetCard medicalTexts2
    , SomeAssetCard scientificTheory3
    , SomeAssetCard archiveOfConduitsGatewayToTindalos4
    , SomeAssetCard archiveOfConduitsGatewayToAcheron4
    , SomeAssetCard archiveOfConduitsGatewayToAldebaran4
    , SomeAssetCard archiveOfConduitsGatewayToParadise4
    , SomeAssetCard prophesiaeProfanaAtlasOfTheUnknowable5
    , --- rogue [eote]
      SomeAssetCard underworldSupport
    , SomeAssetCard theRedClockBrokenButReliable2
    , SomeAssetCard moxie3
    , SomeAssetCard theBlackFan3
    , SomeAssetCard theRedClockBrokenButReliable5
    , --- mystic [eote]
      SomeAssetCard downTheRabbitHole
    , SomeAssetCard dragonPole
    , SomeAssetCard closeTheCircle1
    , SomeAssetCard astronomicalAtlas3
    , SomeAssetCard healingWords3
    , SomeAssetCard grounded3
    , SomeAssetCard trueMagickReworkingReality5
    , --- survivor [eote]
      SomeAssetCard shortSupply
    , SomeAssetCard schoffnersCatalogue
    , SomeAssetCard bandages
    , SomeAssetCard bangleOfJinxes1
    , SomeAssetCard fireExtinguisher3
    , SomeAssetCard plucky3
    , --- guardian/seeker [eote]
      SomeAssetCard medicalStudent
    , SomeAssetCard michaelLeigh5
    , --- guardian/rogue [eote]
      SomeAssetCard oldShotgun2
    , SomeAssetCard quickdrawHolster4
    , --- guardian/mystic [eote]
      SomeAssetCard brandOfCthugha1
    , SomeAssetCard nkosiMabatiEnigmaticWarlock3
    , SomeAssetCard brandOfCthugha4
    , SomeAssetCard cyclopeanHammer5
    , --- guardian/survivor [eote]
      SomeAssetCard sledgehammer
    , SomeAssetCard protectiveGear2
    , SomeAssetCard sledgehammer4
    , --- seeker/rogue [eote]
      SomeAssetCard pocketTelescope
    , SomeAssetCard eonChart1
    , SomeAssetCard geneBeauregard3
    , SomeAssetCard eonChart4
    , --- seeker/mystic [eote]
      SomeAssetCard divination1
    , SomeAssetCard divination4
    , --- seeker/survivor [eote]
      SomeAssetCard professorWilliamWebbFinderOfHiddenConnections
    , SomeAssetCard icePick1
    , SomeAssetCard professorWilliamWebbFinderOfHiddenConnections2
    , SomeAssetCard icePick3
    , --- rogue/mystic [eote]
      SomeAssetCard blur1
    , SomeAssetCard blur4
    , --- rogue/survivor [eote]
      SomeAssetCard unscrupulousLoan3
    , SomeAssetCard preciousMementoFromAFormerLife4
    , SomeAssetCard preciousMementoFromAFutureLife4
    , --- mystic/survivor [eote]
      SomeAssetCard talismanOfProtection
    , SomeAssetCard earthlySerenity1
    , SomeAssetCard enchantedBow2
    , SomeAssetCard earthlySerenity4
    , --- guardian/mystic/survivor [eote]
      SomeAssetCard prophetic3
    , --- guardian/seeker/mystic [eote]
      SomeAssetCard sleuth3
    , --- guardian/rogue/survivor [eote]
      SomeAssetCard bruiser3
    , --- seeker/rogue/survivor [eote]
      SomeAssetCard crafty3
    , --- seeker/rogue/mystic [eote]
      SomeAssetCard antiquary3
    , --- neutral [eote]
      SomeAssetCard inTheThickOfIt
    , SomeAssetCard heavyFurs
    , SomeAssetCard sledDog
    , SomeAssetCard rodOfAnimalism1
    , -- The Scarlet Keys
      --- signature
      SomeAssetCard bonesaw
    , SomeAssetCard woundedBystanderOnDeathsDoorstep
    , SomeAssetCard grapplingHook
    , SomeAssetCard darrellsKodak
    , SomeAssetCard bonnieWalshLoyalAssistant
    , --- guardian [tsk]
      SomeAssetCard huntersArmor
    , SomeAssetCard runicAxe
    , SomeAssetCard guardDog2
    , SomeAssetCard handcuffs2
    , --- seeker [tsk]
      SomeAssetCard alchemicalDistillation
    , SomeAssetCard empiricalHypothesis
    , SomeAssetCard fingerprintKit4
    , --- rogue [tsk]
      SomeAssetCard damningTestimony
    , SomeAssetCard chuckFergus2
    , --- neutral [tsk]
      SomeAssetCard hyperphysicalShotcasterTheoreticalDevice
    , -- The Feast of Hemloch Vale
      --- guardian [tsk]
      SomeAssetCard blessedBlade4
    , --- survivor [tsk]
      SomeAssetCard marinersCompass2
    , -- Return to Night of the Zealot
      --- guardian [rtnotz]
      SomeAssetCard physicalTraining2
    , --- seeker [rtnotz]
      SomeAssetCard hyperawareness2
    , --- rogue [rtnotz]
      SomeAssetCard hardKnocks2
    , --- mystic [rtnotz]
      SomeAssetCard arcaneStudies2
    , --- survivor [rtnotz]
      SomeAssetCard digDeep2
    , SomeAssetCard rabbitsFoot3
    , -- Return to the Dunwich Legacy
      --- guardian [rtdwl]
      SomeAssetCard bandolier2
    , SomeAssetCard blackjack2
    , --- seeker [rtdwl]
      SomeAssetCard strangeSolutionEmpoweringElixir4
    , --- mystic [rtdwl]
      SomeAssetCard riteOfSeeking2
    , SomeAssetCard clarityOfMind3
    , -- Return to the Path to Carcosa
      --- guardian [rtptc]
      SomeAssetCard thirtyTwoColt2
    , --- seeker [rtptc]
      SomeAssetCard archaicGlyphsMarkingsOfIsis3
    , --- rogue [rtptc]
      SomeAssetCard stealth3
    , SomeAssetCard suggestion1
    , --- mystic [rtptc]
      SomeAssetCard alchemicalTransmutation2
    , --- survivor [rtptc]
      SomeAssetCard lantern2
    , SomeAssetCard gravediggersShovel2
    , -- Return to the Forgotten Age
      --- guardian [rttfa]
      SomeAssetCard survivalKnife2
    , --- seeker [rttfa]
      SomeAssetCard ancientStoneTransientThoughts4
    , --- rogue [rttfa]
      SomeAssetCard decoratedSkull3
    , SomeAssetCard coltVestPocket2
    , --- mystic [rttfa]
      SomeAssetCard mistsOfRlyeh2
    , SomeAssetCard theChthonianStone3
    , --- survivor [rttfa]
      SomeAssetCard onYourOwn3_Exceptional
    , --- neutral [rttfa]
      SomeAssetCard backpack2
    , --- weakness [rttfa]
      SomeAssetCard dendromorphosis
    , -- Return to the circle undone
      -- guardian [rttcu]
      SomeAssetCard theStarXvii3
    , SomeAssetCard hallowedMirror3
    , -- seeker [rttcu]
      SomeAssetCard theWorldXxi3
    , SomeAssetCard occultLexicon3
    , -- rogue [rttcu]
      SomeAssetCard knightOfSwords3
    , SomeAssetCard wellConnected3
    , -- mystic [rttcu]
      SomeAssetCard theHierophantV3
    , SomeAssetCard signMagick3
    , -- survivor [rttcu]
      SomeAssetCard nineOfRods3
    , -- neutral [rttcu]
      SomeAssetCard theFool03
    , SomeAssetCard moonPendant2
    , SomeAssetCard observed4
    , -- weakness [rttcu]
      SomeAssetCard theDevilXv
    , -- Investigator Starter Decks
      --- Nathaniel Cho
      SomeAssetCard randallCho
    , SomeAssetCard boxingGloves
    , SomeAssetCard fleshWard
    , SomeAssetCard greteWagner
    , SomeAssetCard relentless
    , SomeAssetCard safeguard
    , SomeAssetCard boxingGloves3
    , SomeAssetCard greteWagner3
    , SomeAssetCard physicalTraining4
    , --- Harvel Walters
      SomeAssetCard vaultOfKnowledge
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
    , --- Winnifred Habbamock
      SomeAssetCard lockpicks
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
    , --- Jacqueline Fine
      SomeAssetCard arbiterOfFates
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
    , --- Stella Clark
      SomeAssetCard eighteenDerringer
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
    , -- Curse of the Rougarou
      --- story [cotr]
      SomeAssetCard ladyEsprit
    , SomeAssetCard bearTrap
    , SomeAssetCard fishingNet
    , SomeAssetCard monstrousTransformation
    , -- Carnevale of Horrors
      --- story [coh]
      SomeAssetCard maskedCarnevaleGoer_17
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
    , -- Murder at the Excelsior Hotel
      --- story [hotel]
      SomeAssetCard bloodstainedDagger
    , SomeAssetCard sergeantMonroe
    , --- Alien Interference
      SomeAssetCard alienDevice
    , --- Excelsior Management
      SomeAssetCard managersKey
    , --- Dark Rituals
      SomeAssetCard tomeOfRituals
    , --- Vile Experiments
      SomeAssetCard sinisterSolution
    , --- Sins of the Past
      SomeAssetCard timeWornLocket
    , -- Parallel Investigators
      --- Read or Die
      SomeAssetCard daisysToteBagAdvanced
    , SomeAssetCard theNecronomiconAdvanced
    , -- Promo
      --- Hour of the Huntress
      SomeAssetCard greenManMedallionHourOfTheHuntress
    , --- Ire of the Void
      SomeAssetCard splitTheAngleIreOfTheVoid
    , --- To Fight the Black Wind
      SomeAssetCard foolishnessFoolishCatOfUlthar
    , --- Blood of Baalshandor
      SomeAssetCard mollyMaxwell
    , --- Dark Revelations
      SomeAssetCard ruthWestmacottDarkRevelations
    , -- Misc
      SomeAssetCard courage
    ]
