{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Id

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (toCardCode a) (AssetId $ toCardId a, toCardOwner a)

lookupAsset :: CardCode -> ((AssetId, Maybe InvestigatorId) -> Asset)
lookupAsset cardCode = case lookup cardCode allAssets of
  Nothing -> error $ "Unknown asset: " <> show cardCode
  Just (SomeAssetCard a) -> Asset <$> cbCardBuilder a

instance FromJSON Asset where
  parseJSON v = flip (withObject "Asset") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withAssetCardCode cCode $ \(_ :: AssetCard a) -> Asset <$> parseJSON @a v

withAssetCardCode
  :: CardCode
  -> (forall a. IsAsset a => AssetCard a -> r)
  -> r
withAssetCardCode cCode f =
  case lookup cCode allAssets of
    Nothing -> error "invalid assets"
    Just (SomeAssetCard a) -> f a

allAssets :: HashMap CardCode SomeAssetCard
allAssets = mapFromList $ map
  (toFst someAssetCardCode)
  [ SomeAssetCard rolands38Special
  , SomeAssetCard daisysToteBag
  , SomeAssetCard theNecronomicon
  , SomeAssetCard heirloomOfHyperborea
  , SomeAssetCard wendysAmulet
  , SomeAssetCard fortyFiveAutomatic
  , SomeAssetCard physicalTraining
  , SomeAssetCard beatCop
  , SomeAssetCard firstAid
  , SomeAssetCard machete
  , SomeAssetCard guardDog
  , SomeAssetCard policeBadge2
  , SomeAssetCard beatCop2
  , SomeAssetCard shotgun4
  , SomeAssetCard magnifyingGlass
  , SomeAssetCard oldBookOfLore
  , SomeAssetCard researchLibrarian
  , SomeAssetCard drMilanChristopher
  , SomeAssetCard hyperawareness
  , SomeAssetCard medicalTexts
  , SomeAssetCard magnifyingGlass1
  , SomeAssetCard discOfItzamna2
  , SomeAssetCard encyclopedia2
  , SomeAssetCard switchblade
  , SomeAssetCard burglary
  , SomeAssetCard pickpocketing
  , SomeAssetCard fortyOneDerringer
  , SomeAssetCard leoDeLuca
  , SomeAssetCard hardKnocks
  , SomeAssetCard leoDeLuca1
  , SomeAssetCard catBurglar1
  , SomeAssetCard forbiddenKnowledge
  , SomeAssetCard holyRosary
  , SomeAssetCard shrivelling
  , SomeAssetCard scrying
  , SomeAssetCard arcaneStudies
  , SomeAssetCard arcaneInitiate
  , SomeAssetCard bookOfShadows3
  , SomeAssetCard grotesqueStatue4
  , SomeAssetCard leatherCoat
  , SomeAssetCard scavenging
  , SomeAssetCard baseballBat
  , SomeAssetCard rabbitsFoot
  , SomeAssetCard strayCat
  , SomeAssetCard digDeep
  , SomeAssetCard aquinnah1
  , SomeAssetCard knife
  , SomeAssetCard flashlight
  , SomeAssetCard bulletproofVest3
  , SomeAssetCard elderSignAmulet3
  , SomeAssetCard litaChantler
  , SomeAssetCard zoeysCross
  , SomeAssetCard jennysTwin45s
  , SomeAssetCard jimsTrumpet
  , SomeAssetCard duke
  , SomeAssetCard blackjack
  , SomeAssetCard laboratoryAssistant
  , SomeAssetCard strangeSolution
  , SomeAssetCard liquidCourage
  , SomeAssetCard hiredMuscle1
  , SomeAssetCard riteOfSeeking
  , SomeAssetCard ritualCandles
  , SomeAssetCard clarityOfMind
  , SomeAssetCard fireAxe
  , SomeAssetCard peterSylvestre
  , SomeAssetCard peterSylvestre2
  , SomeAssetCard kukri
  , SomeAssetCard drHenryArmitage
  , SomeAssetCard alchemicalConcoction
  , SomeAssetCard jazzMulligan
  , SomeAssetCard professorWarrenRice
  , SomeAssetCard peterClover
  , SomeAssetCard drFrancisMorgan
  , SomeAssetCard brotherXavier1
  , SomeAssetCard pathfinder1
  , SomeAssetCard adaptable1
  , SomeAssetCard songOfTheDead2
  , SomeAssetCard fireExtinguisher1
  , SomeAssetCard smokingPipe
  , SomeAssetCard painkillers
  , SomeAssetCard haroldWalsted
  , SomeAssetCard adamLynch
  , SomeAssetCard theNecronomiconOlausWormiusTranslation
  , SomeAssetCard bandolier
  , SomeAssetCard artStudent
  , SomeAssetCard switchblade2
  , SomeAssetCard shrivelling3
  , SomeAssetCard newspaper
  , SomeAssetCard relicHunter3
  , SomeAssetCard charisma3
  , SomeAssetCard helplessPassenger
  , SomeAssetCard keenEye3
  , SomeAssetCard higherEducation3
  , SomeAssetCard loneWolf
  , SomeAssetCard streetwise3
  , SomeAssetCard bloodPact3
  , SomeAssetCard scrapper3
  , SomeAssetCard keyToTheChamber
  , SomeAssetCard zebulonWhateley
  , SomeAssetCard earlSawyer
  , SomeAssetCard powderOfIbnGhazi
  , SomeAssetCard springfieldM19034
  , SomeAssetCard luckyDice2
  , SomeAssetCard alyssaGraham
  , SomeAssetCard riteOfSeeking4
  , SomeAssetCard darkHorse
  , SomeAssetCard esotericFormula
  , SomeAssetCard strangeSolutionRestorativeConcoction4
  , SomeAssetCard strangeSolutionAcidicIchor4
  , SomeAssetCard strangeSolutionFreezingVariant4
  , SomeAssetCard joeyTheRatVigil
  , SomeAssetCard jewelOfAureolus3
  , SomeAssetCard fineClothes
  , SomeAssetCard lightningGun5
  , SomeAssetCard drWilliamTMaleson
  , SomeAssetCard chicagoTypewriter4
  , SomeAssetCard theGoldPocketWatch4
  , SomeAssetCard shrivelling5
  , SomeAssetCard aquinnah3
  , SomeAssetCard tryAndTryAgain3
  , SomeAssetCard theRedGlovedMan5
  , SomeAssetCard sophieInLovingMemory
  , SomeAssetCard sophieItWasAllMyFault
  , SomeAssetCard analyticalMind
  , SomeAssetCard theKingInYellow
  , SomeAssetCard spiritSpeaker
  , SomeAssetCard thirtyTwoColt
  , SomeAssetCard trueGrit
  , SomeAssetCard fieldwork
  , SomeAssetCard archaicGlyphs
  , SomeAssetCard inTheKnow1
  , SomeAssetCard stealth
  , SomeAssetCard lockpicks1
  , SomeAssetCard alchemicalTransmutation
  , SomeAssetCard spiritAthame1
  , SomeAssetCard lantern
  , SomeAssetCard gravediggersShovel
  , SomeAssetCard constanceDumaine
  , SomeAssetCard jordanPerry
  , SomeAssetCard ishimaruHaruko
  , SomeAssetCard sebastienMoreau
  , SomeAssetCard ashleighClarke
  , SomeAssetCard combatTraining1
  , SomeAssetCard scientificTheory1
  , SomeAssetCard knuckleduster
  , SomeAssetCard moxie1
  , SomeAssetCard davidRenfield
  , SomeAssetCard grounded1
  , SomeAssetCard cherishedKeepsake
  , SomeAssetCard plucky1
  , SomeAssetCard mrPeabody
  , SomeAssetCard claspOfBlackOnyx
  , SomeAssetCard theTatteredCloak
  , SomeAssetCard trenchKnife
  , SomeAssetCard charlesRossEsq
  , SomeAssetCard darioElAmin
  , SomeAssetCard bookOfShadows1
  , SomeAssetCard danielChesterfield
  , SomeAssetCard straitjacket
  , SomeAssetCard fortyFiveAutomatic2
  , SomeAssetCard archaicGlyphsGuidingStones3
  , SomeAssetCard archaicGlyphsProphecyForetold3
  , SomeAssetCard pickpocketing2
  , SomeAssetCard madameLabranche
  , SomeAssetCard firstAid3
  , SomeAssetCard fortyOneDerringer2
  , SomeAssetCard scrying3
  , SomeAssetCard stickToThePlan
  , SomeAssetCard arcaneInsight4
  , SomeAssetCard suggestion4
  , SomeAssetCard stHubertsKey
  , SomeAssetCard arcaneInitiate3
  , SomeAssetCard armorOfArdennes5
  , SomeAssetCard charonsObol1
  , SomeAssetCard lupara3
  , SomeAssetCard newspaper2
  , SomeAssetCard keyOfYs
  , SomeAssetCard thePallidMask
  , SomeAssetCard mitchBrown
  , SomeAssetCard jakeWilliams
  , SomeAssetCard finnsTrustyThirtyEight
  , SomeAssetCard theCodexOfAges
  , SomeAssetCard untilTheEndOfTime
  , SomeAssetCard survivalKnife
  , SomeAssetCard venturer
  , SomeAssetCard drElliHorowitz
  , SomeAssetCard ancientStone1
  , SomeAssetCard toothOfEztli
  , SomeAssetCard treasureHunter1
  , SomeAssetCard decoratedSkull
  , SomeAssetCard mistsOfRlyeh
  , SomeAssetCard theChthonianStone
  , SomeAssetCard protectiveIncantation1
  , SomeAssetCard yaotl1
  , SomeAssetCard backpack
  , SomeAssetCard alejandroVela
  , SomeAssetCard relicOfAgesADeviceOfSomeSort
  , SomeAssetCard trackShoes
  , SomeAssetCard meatCleaver
  , SomeAssetCard drawingThin
  , SomeAssetCard occultLexicon
  , SomeAssetCard scrollOfProphecies
  , SomeAssetCard keenEye
  , SomeAssetCard livreDeibon
  , SomeAssetCard physicalTraining2
  , SomeAssetCard hyperawareness2
  , SomeAssetCard hardKnocks2
  , SomeAssetCard arcaneStudies2
  , SomeAssetCard digDeep2
  , SomeAssetCard rabbitsFoot3
  , SomeAssetCard randallCho
  , SomeAssetCard boxingGloves
  , SomeAssetCard fleshWard
  , SomeAssetCard greteWagner
  , SomeAssetCard relentless
  , SomeAssetCard safeguard
  , SomeAssetCard boxingGloves3
  , SomeAssetCard greteWagner3
  , SomeAssetCard physicalTraining4
  , SomeAssetCard arcaneEnlightenment
  , SomeAssetCard celaenoFragments
  , SomeAssetCard encyclopedia
  , SomeAssetCard higherEducation
  , SomeAssetCard whittonGreene
  , SomeAssetCard lockpicks
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
  , SomeAssetCard ladyEsprit
  , SomeAssetCard bearTrap
  , SomeAssetCard fishingNet
  , SomeAssetCard monstrousTransformation
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
  , SomeAssetCard daisysToteBagAdvanced
  , SomeAssetCard theNecronomiconAdvanced
  , SomeAssetCard courage
  ]
