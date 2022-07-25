module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Id
import Arkham.Matcher
import Data.Typeable

data Asset = forall a. IsAsset a => Asset a

instance Eq Asset where
  (Asset (a :: a)) == (Asset (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Asset where
  show (Asset a) = show a

instance ToJSON Asset where
  toJSON (Asset a) = toJSON a

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (toCardCode a) (AssetId $ toCardId a, toCardOwner a)

instance HasAbilities Asset where
  getAbilities (Asset a) = getAbilities a

instance HasModifiersFor Asset where
  getModifiersFor source target (Asset a) = getModifiersFor source target a

instance RunMessage Asset where
  runMessage msg x@(Asset a) = do
    inPlay <- member (toId x) <$> select AnyAsset
    modifiers' <- if inPlay
      then getModifiers (toSource x) (toTarget x)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Asset <$> runMessage msg' a

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs
  toId = toId . toAttrs
  toAttrs (Asset a) = toAttrs a
  overAttrs f (Asset a) = Asset $ overAttrs f a

instance TargetEntity Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupAsset :: CardCode -> ((AssetId, Maybe InvestigatorId) -> Asset)
lookupAsset cardCode =
  fromJustNote ("Unknown asset: " <> show cardCode) $ lookup cardCode allAssets

instance FromJSON Asset where
  parseJSON v = flip (withObject "Asset") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case cCode of
      "01006" -> Asset . Rolands38Special <$> parseJSON v
      "01008" -> Asset . DaisysToteBag <$> parseJSON v
      "01009" -> Asset . TheNecronomicon <$> parseJSON v
      "01012" -> Asset . HeirloomOfHyperborea <$> parseJSON v
      "01014" -> Asset . WendysAmulet <$> parseJSON v
      "01016" -> Asset . FortyFiveAutomatic <$> parseJSON v
      "01017" -> Asset . PhysicalTraining <$> parseJSON v
      "01018" -> Asset . BeatCop <$> parseJSON v
      "01019" -> Asset . FirstAid <$> parseJSON v
      "01020" -> Asset . Machete <$> parseJSON v
      "01021" -> Asset . GuardDog <$> parseJSON v
      "01027" -> Asset . PoliceBadge2 <$> parseJSON v
      "01028" -> Asset . BeatCop2 <$> parseJSON v
      "01029" -> Asset . Shotgun4 <$> parseJSON v
      "01030" -> Asset . MagnifyingGlass <$> parseJSON v
      "01031" -> Asset . OldBookOfLore <$> parseJSON v
      "01032" -> Asset . ResearchLibrarian <$> parseJSON v
      "01033" -> Asset . DrMilanChristopher <$> parseJSON v
      "01034" -> Asset . Hyperawareness <$> parseJSON v
      "01035" -> Asset . MedicalTexts <$> parseJSON v
      "01040" -> Asset . MagnifyingGlass1 <$> parseJSON v
      "01041" -> Asset . DiscOfItzamna2 <$> parseJSON v
      "01042" -> Asset . Encyclopedia2 <$> parseJSON v
      "01044" -> Asset . Switchblade <$> parseJSON v
      "01045" -> Asset . Burglary <$> parseJSON v
      "01046" -> Asset . Pickpocketing <$> parseJSON v
      "01047" -> Asset . FortyOneDerringer <$> parseJSON v
      "01048" -> Asset . LeoDeLuca <$> parseJSON v
      "01049" -> Asset . HardKnocks <$> parseJSON v
      "01054" -> Asset . LeoDeLuca1 <$> parseJSON v
      "01055" -> Asset . CatBurglar1 <$> parseJSON v
      "01058" -> Asset . ForbiddenKnowledge <$> parseJSON v
      "01059" -> Asset . HolyRosary <$> parseJSON v
      "01060" -> Asset . Shrivelling <$> parseJSON v
      "01061" -> Asset . Scrying <$> parseJSON v
      "01062" -> Asset . ArcaneStudies <$> parseJSON v
      "01063" -> Asset . ArcaneInitiate <$> parseJSON v
      "01070" -> Asset . BookOfShadows3 <$> parseJSON v
      "01071" -> Asset . GrotesqueStatue4 <$> parseJSON v
      "01072" -> Asset . LeatherCoat <$> parseJSON v
      "01073" -> Asset . Scavenging <$> parseJSON v
      "01074" -> Asset . BaseballBat <$> parseJSON v
      "01075" -> Asset . RabbitsFoot <$> parseJSON v
      "01076" -> Asset . StrayCat <$> parseJSON v
      "01077" -> Asset . DigDeep <$> parseJSON v
      "01082" -> Asset . Aquinnah1 <$> parseJSON v
      "01086" -> Asset . Knife <$> parseJSON v
      "01087" -> Asset . Flashlight <$> parseJSON v
      "01094" -> Asset . BulletproofVest3 <$> parseJSON v
      "01095" -> Asset . ElderSignAmulet3 <$> parseJSON v
      "01117" -> Asset . LitaChantler <$> parseJSON v
      "02006" -> Asset . ZoeysCross <$> parseJSON v
      "02010" -> Asset . JennysTwin45s <$> parseJSON v
      "02012" -> Asset . JimsTrumpet <$> parseJSON v
      "02014" -> Asset . Duke <$> parseJSON v
      "02016" -> Asset . Blackjack <$> parseJSON v
      "02020" -> Asset . LaboratoryAssistant <$> parseJSON v
      "02021" -> Asset . StrangeSolution <$> parseJSON v
      "02024" -> Asset . LiquidCourage <$> parseJSON v
      "02027" -> Asset . HiredMuscle1 <$> parseJSON v
      "02028" -> Asset . RiteOfSeeking <$> parseJSON v
      "02029" -> Asset . RitualCandles <$> parseJSON v
      "02030" -> Asset . ClarityOfMind <$> parseJSON v
      "02032" -> Asset . FireAxe <$> parseJSON v
      "02033" -> Asset . PeterSylvestre <$> parseJSON v
      "02035" -> Asset . PeterSylvestre2 <$> parseJSON v
      "02036" -> Asset . Kukri <$> parseJSON v
      "02040" -> Asset . DrHenryArmitage <$> parseJSON v
      "02059" -> Asset . AlchemicalConcoction <$> parseJSON v
      "02060" -> Asset . JazzMulligan <$> parseJSON v
      "02061" -> Asset . ProfessorWarrenRice <$> parseJSON v
      "02079" -> Asset . PeterClover <$> parseJSON v
      "02080" -> Asset . DrFrancisMorgan <$> parseJSON v
      "02106" -> Asset . BrotherXavier1 <$> parseJSON v
      "02108" -> Asset . Pathfinder1 <$> parseJSON v
      "02110" -> Asset . Adaptable1 <$> parseJSON v
      "02112" -> Asset . SongOfTheDead2 <$> parseJSON v
      "02114" -> Asset . FireExtinguisher1 <$> parseJSON v
      "02116" -> Asset . SmokingPipe <$> parseJSON v
      "02117" -> Asset . Painkillers <$> parseJSON v
      "02138" -> Asset . HaroldWalsted <$> parseJSON v
      "02139" -> Asset . AdamLynch <$> parseJSON v
      "02140" -> Asset . TheNecronomiconOlausWormiusTranslation <$> parseJSON v
      "02147" -> Asset . Bandolier <$> parseJSON v
      "02149" -> Asset . ArtStudent <$> parseJSON v
      "02152" -> Asset . Switchblade2 <$> parseJSON v
      "02154" -> Asset . Shrivelling3 <$> parseJSON v
      "02155" -> Asset . Newspaper <$> parseJSON v
      "02157" -> Asset . RelicHunter3 <$> parseJSON v
      "02158" -> Asset . Charisma3 <$> parseJSON v
      "02179" -> Asset . HelplessPassenger <$> parseJSON v
      "02185" -> Asset . KeenEye3 <$> parseJSON v
      "02187" -> Asset . HigherEducation3 <$> parseJSON v
      "02188" -> Asset . LoneWolf <$> parseJSON v
      "02189" -> Asset . Streetwise3 <$> parseJSON v
      "02191" -> Asset . BloodPact3 <$> parseJSON v
      "02193" -> Asset . Scrapper3 <$> parseJSON v
      "02215" -> Asset . KeyToTheChamber <$> parseJSON v
      "02217" -> Asset . ZebulonWhateley <$> parseJSON v
      "02218" -> Asset . EarlSawyer <$> parseJSON v
      "02219" -> Asset . PowderOfIbnGhazi <$> parseJSON v
      "02226" -> Asset . SpringfieldM19034 <$> parseJSON v
      "02230" -> Asset . LuckyDice2 <$> parseJSON v
      "02232" -> Asset . AlyssaGraham <$> parseJSON v
      "02233" -> Asset . RiteOfSeeking4 <$> parseJSON v
      "02234" -> Asset . DarkHorse <$> parseJSON v
      "02254" -> Asset . EsotericFormula <$> parseJSON v
      "02262" -> Asset . StrangeSolutionRestorativeConcoction4 <$> parseJSON v
      "02263" -> Asset . StrangeSolutionAcidicIchor4 <$> parseJSON v
      "02264" -> Asset . StrangeSolutionFreezingVariant4 <$> parseJSON v
      "02265" -> Asset . JoeyTheRatVigil <$> parseJSON v
      "02269" -> Asset . JewelOfAureolus3 <$> parseJSON v
      "02272" -> Asset . FineClothes <$> parseJSON v
      "02301" -> Asset . LightningGun5 <$> parseJSON v
      "02302" -> Asset . DrWilliamTMaleson <$> parseJSON v
      "02304" -> Asset . ChicagoTypewriter4 <$> parseJSON v
      "02305" -> Asset . TheGoldPocketWatch4 <$> parseJSON v
      "02306" -> Asset . Shrivelling5 <$> parseJSON v
      "02308" -> Asset . Aquinnah3 <$> parseJSON v
      "02309" -> Asset . TryAndTryAgain3 <$> parseJSON v
      "02310" -> Asset . TheRedGlovedMan5 <$> parseJSON v
      "03009" -> Asset . SophieInLovingMemory <$> parseJSON v
      "03009b" -> Asset . SophieItWasAllMyFault <$> parseJSON v
      "03010" -> Asset . AnalyticalMind <$> parseJSON v
      "03011" -> Asset . TheKingInYellow <$> parseJSON v
      "03014" -> Asset . SpiritSpeaker <$> parseJSON v
      "03020" -> Asset . ThirtyTwoColt <$> parseJSON v
      "03021" -> Asset . TrueGrit <$> parseJSON v
      "03024" -> Asset . Fieldwork <$> parseJSON v
      "03025" -> Asset . ArchaicGlyphs <$> parseJSON v
      "03027" -> Asset . InTheKnow1 <$> parseJSON v
      "03028" -> Asset . Stealth <$> parseJSON v
      "03031" -> Asset . Lockpicks1 <$> parseJSON v
      "03032" -> Asset . AlchemicalTransmutation <$> parseJSON v
      "03035" -> Asset . SpiritAthame1 <$> parseJSON v
      "03036" -> Asset . Lantern <$> parseJSON v
      "03037" -> Asset . GravediggersShovel <$> parseJSON v
      "03076" -> Asset . ConstanceDumaine <$> parseJSON v
      "03077" -> Asset . JordanPerry <$> parseJSON v
      "03078" -> Asset . IshimaruHaruko <$> parseJSON v
      "03079" -> Asset . SebastienMoreau <$> parseJSON v
      "03080" -> Asset . AshleighClarke <$> parseJSON v
      "03107" -> Asset . CombatTraining1 <$> parseJSON v
      "03109" -> Asset . ScientificTheory1 <$> parseJSON v
      "03110" -> Asset . Knuckleduster <$> parseJSON v
      "03111" -> Asset . Moxie1 <$> parseJSON v
      "03112" -> Asset . DavidRenfield <$> parseJSON v
      "03113" -> Asset . Grounded1 <$> parseJSON v
      "03114" -> Asset . CherishedKeepsake <$> parseJSON v
      "03115" -> Asset . Plucky1 <$> parseJSON v
      "03141" -> Asset . MrPeabody <$> parseJSON v
      "03142" -> Asset . ClaspOfBlackOnyx <$> parseJSON v
      "03143" -> Asset . TheTatteredCloak <$> parseJSON v
      "03147" -> Asset . TrenchKnife <$> parseJSON v
      "03149" -> Asset . CharlesRossEsq <$> parseJSON v
      "03151" -> Asset . DarioElAmin <$> parseJSON v
      "03154" -> Asset . BookOfShadows1 <$> parseJSON v
      "03182a" -> Asset . DanielChesterfield <$> parseJSON v
      "03185" -> Asset . Straitjacket <$> parseJSON v
      "03190" -> Asset . FortyFiveAutomatic2 <$> parseJSON v
      "03192" -> Asset . ArchaicGlyphsGuidingStones3 <$> parseJSON v
      "03193" -> Asset . ArchaicGlyphsProphecyForetold3 <$> parseJSON v
      "03195" -> Asset . Pickpocketing2 <$> parseJSON v
      "03198" -> Asset . MadameLabranche <$> parseJSON v
      "03230" -> Asset . FirstAid3 <$> parseJSON v
      "03234" -> Asset . FortyOneDerringer2 <$> parseJSON v
      "03236" -> Asset . Scrying3 <$> parseJSON v
      "03264" -> Asset . StickToThePlan <$> parseJSON v
      "03266" -> Asset . ArcaneInsight4 <$> parseJSON v
      "03268" -> Asset . Suggestion4 <$> parseJSON v
      "03269" -> Asset . StHubertsKey <$> parseJSON v
      "03271" -> Asset . ArcaneInitiate3 <$> parseJSON v
      "03305" -> Asset . ArmorOfArdennes5 <$> parseJSON v
      "03308" -> Asset . CharonsObol1 <$> parseJSON v
      "03309" -> Asset . Lupara3 <$> parseJSON v
      "03313" -> Asset . Newspaper2 <$> parseJSON v
      "03315" -> Asset . KeyOfYs <$> parseJSON v
      "03321b" -> Asset . ThePallidMask <$> parseJSON v
      "04006" -> Asset . MitchBrown <$> parseJSON v
      "04008" -> Asset . JakeWilliams <$> parseJSON v
      "04011" -> Asset . FinnsTrustyThirtyEight <$> parseJSON v
      "04013" -> Asset . TheCodexOfAges <$> parseJSON v
      "04015" -> Asset . UntilTheEndOfTime <$> parseJSON v
      "04017" -> Asset . SurvivalKnife <$> parseJSON v
      "04018" -> Asset . Venturer <$> parseJSON v
      "04021" -> Asset . DrElliHorowitz <$> parseJSON v
      "04022" -> Asset . AncientStone1 <$> parseJSON v
      "04023" -> Asset . ToothOfEztli <$> parseJSON v
      "04025" -> Asset . TreasureHunter1 <$> parseJSON v
      "04026" -> Asset . DecoratedSkull <$> parseJSON v
      "04029" -> Asset . MistsOfRlyeh <$> parseJSON v
      "04030" -> Asset . TheChthonianStone <$> parseJSON v
      "04031" -> Asset . ProtectiveIncantation1 <$> parseJSON v
      "04035" -> Asset . Yaotl1 <$> parseJSON v
      "04037" -> Asset . Backpack <$> parseJSON v
      "05036" -> Asset . TrackShoes <$> parseJSON v
      "05114" -> Asset . MeatCleaver <$> parseJSON v
      "05159" -> Asset . DrawingThin <$> parseJSON v
      "05316" -> Asset . OccultLexicon <$> parseJSON v
      "06116" -> Asset . ScrollOfProphecies <$> parseJSON v
      "07152" -> Asset . KeenEye <$> parseJSON v
      "08005" -> Asset . LivreDeibon <$> parseJSON v
      "50001" -> Asset . PhysicalTraining2 <$> parseJSON v
      "50003" -> Asset . Hyperawareness2 <$> parseJSON v
      "50005" -> Asset . HardKnocks2 <$> parseJSON v
      "50007" -> Asset . ArcaneStudies2 <$> parseJSON v
      "50009" -> Asset . DigDeep2 <$> parseJSON v
      "50010" -> Asset . RabbitsFoot3 <$> parseJSON v
      "60102" -> Asset . RandallCho <$> parseJSON v
      "60105" -> Asset . BoxingGloves <$> parseJSON v
      "60106" -> Asset . FleshWard <$> parseJSON v
      "60107" -> Asset . GreteWagner <$> parseJSON v
      "60109" -> Asset . Relentless <$> parseJSON v
      "60110" -> Asset . Safeguard <$> parseJSON v
      "60127" -> Asset . BoxingGloves3 <$> parseJSON v
      "60128" -> Asset . GreteWagner3 <$> parseJSON v
      "60131" -> Asset . PhysicalTraining4 <$> parseJSON v
      "60205" -> Asset . ArcaneEnlightenment <$> parseJSON v
      "60206" -> Asset . CelaenoFragments <$> parseJSON v
      "60208" -> Asset . Encyclopedia <$> parseJSON v
      "60211" -> Asset . HigherEducation <$> parseJSON v
      "60213" -> Asset . WhittonGreene <$> parseJSON v
      "60305" -> Asset . Lockpicks <$> parseJSON v
      "60505" -> Asset . EighteenDerringer <$> parseJSON v
      "60506" -> Asset . GrimmsFairyTales <$> parseJSON v
      "60507" -> Asset . OldKeyring <$> parseJSON v
      "60508" -> Asset . GrannyOrne <$> parseJSON v
      "60509" -> Asset . MysteriousRaven <$> parseJSON v
      "60511" -> Asset . Scrapper <$> parseJSON v
      "60520" -> Asset . CherishedKeepsake1 <$> parseJSON v
      "60521" -> Asset . LeatherCoat1 <$> parseJSON v
      "60522" -> Asset . EighteenDerringer2 <$> parseJSON v
      "60527" -> Asset . GrannyOrne3 <$> parseJSON v
      "60529" -> Asset . Chainsaw4 <$> parseJSON v
      "60530" -> Asset . QuickLearner4 <$> parseJSON v
      "60531" -> Asset . DejaVu5 <$> parseJSON v
      "81019" -> Asset . LadyEsprit <$> parseJSON v
      "81020" -> Asset . BearTrap <$> parseJSON v
      "81021" -> Asset . FishingNet <$> parseJSON v
      "81030" -> Asset . MonstrousTransformation <$> parseJSON v
      "82017b" -> Asset . MaskedCarnevaleGoer_17 <$> parseJSON v
      "82018b" -> Asset . MaskedCarnevaleGoer_18 <$> parseJSON v
      "82019b" -> Asset . MaskedCarnevaleGoer_19 <$> parseJSON v
      "82020b" -> Asset . MaskedCarnevaleGoer_20 <$> parseJSON v
      "82021" -> Asset . InnocentReveler <$> parseJSON v
      "82021b" -> Asset . MaskedCarnevaleGoer_21 <$> parseJSON v
      "82022" -> Asset . AbbessAllegriaDiBiase <$> parseJSON v
      "82023" -> Asset . Bauta <$> parseJSON v
      "82024" -> Asset . MedicoDellaPeste <$> parseJSON v
      "82025" -> Asset . Pantalone <$> parseJSON v
      "82026" -> Asset . GildedVolto <$> parseJSON v
      "90002" -> Asset . DaisysToteBagAdvanced <$> parseJSON v
      "90003" -> Asset . TheNecronomiconAdvanced <$> parseJSON v
      "xcourage" -> Asset . Courage <$> parseJSON v
      _ -> error "invalid asset"

allAssets :: HashMap CardCode ((AssetId, Maybe InvestigatorId) -> Asset)
allAssets = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  [ Asset <$> rolands38Special
  , Asset <$> daisysToteBag
  , Asset <$> theNecronomicon
  , Asset <$> heirloomOfHyperborea
  , Asset <$> wendysAmulet
  , Asset <$> fortyFiveAutomatic
  , Asset <$> physicalTraining
  , Asset <$> beatCop
  , Asset <$> firstAid
  , Asset <$> machete
  , Asset <$> guardDog
  , Asset <$> policeBadge2
  , Asset <$> beatCop2
  , Asset <$> shotgun4
  , Asset <$> magnifyingGlass
  , Asset <$> oldBookOfLore
  , Asset <$> researchLibrarian
  , Asset <$> drMilanChristopher
  , Asset <$> hyperawareness
  , Asset <$> medicalTexts
  , Asset <$> magnifyingGlass1
  , Asset <$> discOfItzamna2
  , Asset <$> encyclopedia2
  , Asset <$> switchblade
  , Asset <$> burglary
  , Asset <$> pickpocketing
  , Asset <$> fortyOneDerringer
  , Asset <$> leoDeLuca
  , Asset <$> hardKnocks
  , Asset <$> leoDeLuca1
  , Asset <$> catBurglar1
  , Asset <$> forbiddenKnowledge
  , Asset <$> holyRosary
  , Asset <$> shrivelling
  , Asset <$> scrying
  , Asset <$> arcaneStudies
  , Asset <$> arcaneInitiate
  , Asset <$> bookOfShadows3
  , Asset <$> grotesqueStatue4
  , Asset <$> leatherCoat
  , Asset <$> scavenging
  , Asset <$> baseballBat
  , Asset <$> rabbitsFoot
  , Asset <$> strayCat
  , Asset <$> digDeep
  , Asset <$> aquinnah1
  , Asset <$> knife
  , Asset <$> flashlight
  , Asset <$> bulletproofVest3
  , Asset <$> elderSignAmulet3
  , Asset <$> litaChantler
  , Asset <$> zoeysCross
  , Asset <$> jennysTwin45s
  , Asset <$> jimsTrumpet
  , Asset <$> duke
  , Asset <$> blackjack
  , Asset <$> laboratoryAssistant
  , Asset <$> strangeSolution
  , Asset <$> liquidCourage
  , Asset <$> hiredMuscle1
  , Asset <$> riteOfSeeking
  , Asset <$> ritualCandles
  , Asset <$> clarityOfMind
  , Asset <$> fireAxe
  , Asset <$> peterSylvestre
  , Asset <$> peterSylvestre2
  , Asset <$> kukri
  , Asset <$> drHenryArmitage
  , Asset <$> alchemicalConcoction
  , Asset <$> jazzMulligan
  , Asset <$> professorWarrenRice
  , Asset <$> peterClover
  , Asset <$> drFrancisMorgan
  , Asset <$> brotherXavier1
  , Asset <$> pathfinder1
  , Asset <$> adaptable1
  , Asset <$> songOfTheDead2
  , Asset <$> fireExtinguisher1
  , Asset <$> smokingPipe
  , Asset <$> painkillers
  , Asset <$> haroldWalsted
  , Asset <$> adamLynch
  , Asset <$> theNecronomiconOlausWormiusTranslation
  , Asset <$> bandolier
  , Asset <$> artStudent
  , Asset <$> switchblade2
  , Asset <$> shrivelling3
  , Asset <$> newspaper
  , Asset <$> relicHunter3
  , Asset <$> charisma3
  , Asset <$> helplessPassenger
  , Asset <$> keenEye3
  , Asset <$> higherEducation3
  , Asset <$> loneWolf
  , Asset <$> streetwise3
  , Asset <$> bloodPact3
  , Asset <$> scrapper3
  , Asset <$> keyToTheChamber
  , Asset <$> zebulonWhateley
  , Asset <$> earlSawyer
  , Asset <$> powderOfIbnGhazi
  , Asset <$> springfieldM19034
  , Asset <$> luckyDice2
  , Asset <$> alyssaGraham
  , Asset <$> riteOfSeeking4
  , Asset <$> darkHorse
  , Asset <$> esotericFormula
  , Asset <$> strangeSolutionRestorativeConcoction4
  , Asset <$> strangeSolutionAcidicIchor4
  , Asset <$> strangeSolutionFreezingVariant4
  , Asset <$> joeyTheRatVigil
  , Asset <$> jewelOfAureolus3
  , Asset <$> fineClothes
  , Asset <$> lightningGun5
  , Asset <$> drWilliamTMaleson
  , Asset <$> chicagoTypewriter4
  , Asset <$> theGoldPocketWatch4
  , Asset <$> shrivelling5
  , Asset <$> aquinnah3
  , Asset <$> tryAndTryAgain3
  , Asset <$> theRedGlovedMan5
  , Asset <$> sophieInLovingMemory
  , Asset <$> sophieItWasAllMyFault
  , Asset <$> analyticalMind
  , Asset <$> theKingInYellow
  , Asset <$> spiritSpeaker
  , Asset <$> thirtyTwoColt
  , Asset <$> trueGrit
  , Asset <$> fieldwork
  , Asset <$> archaicGlyphs
  , Asset <$> inTheKnow1
  , Asset <$> stealth
  , Asset <$> lockpicks1
  , Asset <$> alchemicalTransmutation
  , Asset <$> spiritAthame1
  , Asset <$> lantern
  , Asset <$> gravediggersShovel
  , Asset <$> constanceDumaine
  , Asset <$> jordanPerry
  , Asset <$> ishimaruHaruko
  , Asset <$> sebastienMoreau
  , Asset <$> ashleighClarke
  , Asset <$> combatTraining1
  , Asset <$> scientificTheory1
  , Asset <$> knuckleduster
  , Asset <$> moxie1
  , Asset <$> davidRenfield
  , Asset <$> grounded1
  , Asset <$> cherishedKeepsake
  , Asset <$> plucky1
  , Asset <$> mrPeabody
  , Asset <$> claspOfBlackOnyx
  , Asset <$> theTatteredCloak
  , Asset <$> trenchKnife
  , Asset <$> charlesRossEsq
  , Asset <$> darioElAmin
  , Asset <$> bookOfShadows1
  , Asset <$> danielChesterfield
  , Asset <$> straitjacket
  , Asset <$> fortyFiveAutomatic2
  , Asset <$> archaicGlyphsGuidingStones3
  , Asset <$> archaicGlyphsProphecyForetold3
  , Asset <$> pickpocketing2
  , Asset <$> madameLabranche
  , Asset <$> firstAid3
  , Asset <$> fortyOneDerringer2
  , Asset <$> scrying3
  , Asset <$> stickToThePlan
  , Asset <$> arcaneInsight4
  , Asset <$> suggestion4
  , Asset <$> stHubertsKey
  , Asset <$> arcaneInitiate3
  , Asset <$> armorOfArdennes5
  , Asset <$> charonsObol1
  , Asset <$> lupara3
  , Asset <$> newspaper2
  , Asset <$> keyOfYs
  , Asset <$> thePallidMask
  , Asset <$> mitchBrown
  , Asset <$> jakeWilliams
  , Asset <$> finnsTrustyThirtyEight
  , Asset <$> theCodexOfAges
  , Asset <$> untilTheEndOfTime
  , Asset <$> survivalKnife
  , Asset <$> venturer
  , Asset <$> drElliHorowitz
  , Asset <$> ancientStone1
  , Asset <$> toothOfEztli
  , Asset <$> treasureHunter1
  , Asset <$> decoratedSkull
  , Asset <$> mistsOfRlyeh
  , Asset <$> theChthonianStone
  , Asset <$> protectiveIncantation1
  , Asset <$> yaotl1
  , Asset <$> backpack
  , Asset <$> trackShoes
  , Asset <$> meatCleaver
  , Asset <$> drawingThin
  , Asset <$> occultLexicon
  , Asset <$> scrollOfProphecies
  , Asset <$> keenEye
  , Asset <$> livreDeibon
  , Asset <$> physicalTraining2
  , Asset <$> hyperawareness2
  , Asset <$> hardKnocks2
  , Asset <$> arcaneStudies2
  , Asset <$> digDeep2
  , Asset <$> rabbitsFoot3
  , Asset <$> randallCho
  , Asset <$> boxingGloves
  , Asset <$> fleshWard
  , Asset <$> greteWagner
  , Asset <$> relentless
  , Asset <$> safeguard
  , Asset <$> boxingGloves3
  , Asset <$> greteWagner3
  , Asset <$> physicalTraining4
  , Asset <$> arcaneEnlightenment
  , Asset <$> celaenoFragments
  , Asset <$> encyclopedia
  , Asset <$> higherEducation
  , Asset <$> whittonGreene
  , Asset <$> lockpicks
  , Asset <$> eighteenDerringer
  , Asset <$> grimmsFairyTales
  , Asset <$> oldKeyring
  , Asset <$> grannyOrne
  , Asset <$> mysteriousRaven
  , Asset <$> scrapper
  , Asset <$> cherishedKeepsake1
  , Asset <$> leatherCoat1
  , Asset <$> eighteenDerringer2
  , Asset <$> grannyOrne3
  , Asset <$> chainsaw4
  , Asset <$> quickLearner4
  , Asset <$> dejaVu5
  , Asset <$> ladyEsprit
  , Asset <$> bearTrap
  , Asset <$> fishingNet
  , Asset <$> monstrousTransformation
  , Asset <$> maskedCarnevaleGoer_17
  , Asset <$> maskedCarnevaleGoer_18
  , Asset <$> maskedCarnevaleGoer_19
  , Asset <$> maskedCarnevaleGoer_20
  , Asset <$> innocentReveler
  , Asset <$> maskedCarnevaleGoer_21
  , Asset <$> abbessAllegriaDiBiase
  , Asset <$> bauta
  , Asset <$> medicoDellaPeste
  , Asset <$> pantalone
  , Asset <$> gildedVolto
  , Asset <$> daisysToteBagAdvanced
  , Asset <$> theNecronomiconAdvanced
  , Asset <$> courage
  ]
