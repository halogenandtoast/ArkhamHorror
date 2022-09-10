{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Id

createAsset :: IsCard a => a -> Asset
createAsset a =
  lookupAsset (toCardCode a) (AssetId $ toCardId a, toCardOwner a)

lookupAsset :: CardCode -> ((AssetId, Maybe InvestigatorId) -> Asset)
lookupAsset cardCode = case lookup cardCode allAssets of
  Nothing -> error $ "Unknown asset: " <> show cardCode
  Just (SomeAssetCard a) -> Asset <$> cbCardBuilder a

instance FromJSON Asset where
  parseJSON v = flip (withObject "Asset") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withAssetCardCode cCode $ \(_ :: AssetCard a) -> Asset <$> parseJSON @a v

withAssetCardCode
  :: CardCode -> (forall a . IsAsset a => AssetCard a -> r) -> r
withAssetCardCode cCode f = case lookup cCode allAssets of
  Nothing -> error "invalid assets"
  Just (SomeAssetCard a) -> f a

allAssets :: HashMap CardCode SomeAssetCard
allAssets = mapFromList $ map
  (toFst someAssetCardCode)
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
  -- Udimensioned and Unseen
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
  , SomeAssetCard stickToThePlan
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
  -- The Circle Undone
  --- survivor [tcu]
  , SomeAssetCard trackShoes
  -- The Secret Name
  --- survivor [tsn]
  , SomeAssetCard meatCleaver
  -- The Wages of Sin
  --- survivor [wos]
  , SomeAssetCard drawingThin
  -- Before the Black Throne
  --- seeker [bbt]
  , SomeAssetCard occultLexicon
  -- The Search for Kadath
  --- mystic [sfk]
  , SomeAssetCard scrollOfProphecies
  -- Devil Reef
  --- gurdian [def]
  , SomeAssetCard keenEye
  -- Edge of the Earth
  --- signature [eote]
  , SomeAssetCard livreDeibon
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
  , SomeAssetCard arcaneEnlightenment
  , SomeAssetCard celaenoFragments
  , SomeAssetCard encyclopedia
  , SomeAssetCard higherEducation
  , SomeAssetCard whittonGreene
  --- Winnifred Habbamock
  , SomeAssetCard lockpicks
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
  -- Misc
  , SomeAssetCard courage
  ]
