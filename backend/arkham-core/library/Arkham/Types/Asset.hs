module Arkham.Types.Asset
  ( module Arkham.Types.Asset
  , module X
  ) where

import Arkham.Prelude

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Cards
import Arkham.Types.Asset.Class as X
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Trait (Trait)

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (toCardCode a) (AssetId $ toCardId a)

data Asset
  = BaseAsset' BaseAsset
  | AbbessAllegriaDiBiase' AbbessAllegriaDiBiase
  | AdamLynch' AdamLynch
  | Adaptable1' Adaptable1
  | AlchemicalConcoction' AlchemicalConcoction
  | AlyssaGraham' AlyssaGraham
  | Aquinnah1' Aquinnah1
  | Aquinnah3' Aquinnah3
  | ArcaneEnlightenment' ArcaneEnlightenment
  | ArcaneInitiate' ArcaneInitiate
  | ArcaneStudies' ArcaneStudies
  | ArcaneStudies2' ArcaneStudies2
  | ArtStudent' ArtStudent
  | Bandolier' Bandolier
  | BaseballBat' BaseballBat
  | BearTrap' BearTrap
  | BeatCop' BeatCop
  | BeatCop2' BeatCop2
  | Blackjack' Blackjack
  | BloodPact3' BloodPact3
  | BookOfShadows3' BookOfShadows3
  | BrotherXavier1' BrotherXavier1
  | BulletproofVest3' BulletproofVest3
  | Burglary' Burglary
  | CatBurglar1' CatBurglar1
  | CelaenoFragments' CelaenoFragments
  | Charisma3' Charisma3
  | ChicagoTypewriter4' ChicagoTypewriter4
  | ClarityOfMind' ClarityOfMind
  | DaisysToteBag' DaisysToteBag
  | DaisysToteBagAdvanced' DaisysToteBagAdvanced
  | DarkHorse' DarkHorse
  | DigDeep' DigDeep
  | DigDeep2' DigDeep2
  | DiscOfItzamna2' DiscOfItzamna2
  | DrFrancisMorgan' DrFrancisMorgan
  | DrHenryArmitage' DrHenryArmitage
  | DrMilanChristopher' DrMilanChristopher
  | DrWilliamTMaleson' DrWilliamTMaleson
  | Duke' Duke
  | EarlSawyer' EarlSawyer
  | ElderSignAmulet3' ElderSignAmulet3
  | Encyclopedia' Encyclopedia
  | Encyclopedia2' Encyclopedia2
  | EsotericFormula' EsotericFormula
  | FineClothes' FineClothes
  | FireAxe' FireAxe
  | FireExtinguisher1' FireExtinguisher1
  | FirstAid' FirstAid
  | FishingNet' FishingNet
  | Flashlight' Flashlight
  | ForbiddenKnowledge' ForbiddenKnowledge
  | FortyFiveAutomatic' FortyFiveAutomatic
  | FortyOneDerringer' FortyOneDerringer
  | GrotesqueStatue4' GrotesqueStatue4
  | GuardDog' GuardDog
  | HardKnocks' HardKnocks
  | HardKnocks2' HardKnocks2
  | HaroldWalsted' HaroldWalsted
  | HeirloomOfHyperborea' HeirloomOfHyperborea
  | HelplessPassenger' HelplessPassenger
  | HigherEducation' HigherEducation
  | HigherEducation3' HigherEducation3
  | HiredMuscle1' HiredMuscle1
  | HolyRosary' HolyRosary
  | Hyperawareness' Hyperawareness
  | Hyperawareness2' Hyperawareness2
  | InnocentReveler' InnocentReveler
  | JazzMulligan' JazzMulligan
  | JennysTwin45s' JennysTwin45s
  | JewelOfAureolus3' JewelOfAureolus3
  | JimsTrumpet' JimsTrumpet
  | JoeyTheRatVigil' JoeyTheRatVigil
  | KeenEye' KeenEye
  | KeenEye3' KeenEye3
  | KeyToTheChamber' KeyToTheChamber
  | Knife' Knife
  | Kukri' Kukri
  | LaboratoryAssistant' LaboratoryAssistant
  | LadyEsprit' LadyEsprit
  | LeatherCoat' LeatherCoat
  | LeoDeLuca' LeoDeLuca
  | LeoDeLuca1' LeoDeLuca1
  | LightningGun5' LightningGun5
  | LiquidCourage' LiquidCourage
  | LitaChantler' LitaChantler
  | LoneWolf' LoneWolf
  | LuckyDice2' LuckyDice2
  | Machete' Machete
  | MagnifyingGlass' MagnifyingGlass
  | MagnifyingGlass1' MagnifyingGlass1
  | MaskedCarnevaleGoer_17' MaskedCarnevaleGoer_17
  | MaskedCarnevaleGoer_18' MaskedCarnevaleGoer_18
  | MaskedCarnevaleGoer_19' MaskedCarnevaleGoer_19
  | MaskedCarnevaleGoer_20' MaskedCarnevaleGoer_20
  | MaskedCarnevaleGoer_21' MaskedCarnevaleGoer_21
  | MedicalTexts' MedicalTexts
  | MonstrousTransformation' MonstrousTransformation
  | Newspaper' Newspaper
  | OccultLexicon' OccultLexicon
  | OldBookOfLore' OldBookOfLore
  | Painkillers' Painkillers
  | Pathfinder1' Pathfinder1
  | PeterClover' PeterClover
  | PeterSylvestre' PeterSylvestre
  | PeterSylvestre2' PeterSylvestre2
  | PhysicalTraining' PhysicalTraining
  | PhysicalTraining2' PhysicalTraining2
  | Pickpocketing' Pickpocketing
  | PoliceBadge2' PoliceBadge2
  | PowderOfIbnGhazi' PowderOfIbnGhazi
  | ProfessorWarrenRice' ProfessorWarrenRice
  | RabbitsFoot' RabbitsFoot
  | RabbitsFoot3' RabbitsFoot3
  | RelicHunter3' RelicHunter3
  | ResearchLibrarian' ResearchLibrarian
  | RiteOfSeeking' RiteOfSeeking
  | RiteOfSeeking4' RiteOfSeeking4
  | RitualCandles' RitualCandles
  | Rolands38Special' Rolands38Special
  | Scavenging' Scavenging
  | Scrapper3' Scrapper3
  | ScrollOfProphecies' ScrollOfProphecies
  | Scrying' Scrying
  | Shotgun4' Shotgun4
  | Shrivelling' Shrivelling
  | Shrivelling3' Shrivelling3
  | Shrivelling5' Shrivelling5
  | SmokingPipe' SmokingPipe
  | SongOfTheDead2' SongOfTheDead2
  | SpringfieldM19034' SpringfieldM19034
  | StrangeSolution' StrangeSolution
  | StrangeSolutionAcidicIchor4' StrangeSolutionAcidicIchor4
  | StrangeSolutionFreezingVariant4' StrangeSolutionFreezingVariant4
  | StrangeSolutionRestorativeConcoction4' StrangeSolutionRestorativeConcoction4
  | StrayCat' StrayCat
  | Streetwise3' Streetwise3
  | Switchblade' Switchblade
  | Switchblade2' Switchblade2
  | TheGoldPocketWatch4' TheGoldPocketWatch4
  | TheNecronomicon' TheNecronomicon
  | TheNecronomiconAdvanced' TheNecronomiconAdvanced
  | TheNecronomiconOlausWormiusTranslation' TheNecronomiconOlausWormiusTranslation
  | TheRedGlovedMan5' TheRedGlovedMan5
  | ToothOfEztli' ToothOfEztli
  | TryAndTryAgain3' TryAndTryAgain3
  | WendysAmulet' WendysAmulet
  | WhittonGreene' WhittonGreene
  | ZebulonWhateley' ZebulonWhateley
  | ZoeysCross' ZoeysCross
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (ActionRunner env , HasSkillTest env) => HasActions env Asset where
  getActions iid window x = do
    inPlay <- member (toId x) <$> getSet ()
    modifiers' <- if inPlay
      then getModifiersFor (toSource x) (toTarget x) ()
      else pure []
    if any isBlank modifiers'
      then getActions iid window (toAttrs x)
      else defaultGetActions iid window x

instance
  ( HasId LocationId env InvestigatorId
  , HasId CardCode env EnemyId
  , HasId (Maybe LocationId) env LocationMatcher
  , HasCount ResourceCount env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount ClueCount env EnemyId
  , HasCount ClueCount env InvestigatorId
  , HasCount AssetCount env (InvestigatorId, [Trait])
  , HasSet Trait env LocationId
  , HasSkillTest env
  )
  => HasModifiersFor env Asset where
  getModifiersFor = genericGetModifiersFor

instance
  ( HasActions env ActionType
  , HasList HandCard env InvestigatorId
  , HasCount DoomCount env AssetId
  , HasCount DoomCount env InvestigatorId
  , HasList DiscardedPlayerCard env InvestigatorId
  , HasList CommittedCard env InvestigatorId
  , HasId LeadInvestigatorId env ()
  , AssetRunner env
  )
  => RunMessage env Asset where
  runMessage msg x = do
    inPlay <- member (toId x) <$> getSet ()
    modifiers' <- if inPlay
      then getModifiersFor (toSource x) (toTarget x) ()
      else pure []
    let msg' = if any isBlank modifiers' then Blanked msg else msg
    defaultRunMessage msg' x

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs

instance Named Asset where
  toName = toName . toAttrs

instance HasName env Asset where
  getName = pure . toName

instance TargetEntity Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardDef Asset where
  toCardDef = toCardDef . toAttrs

instance IsCard Asset where
  toCardId = toCardId . toAttrs

newtype BaseAsset = BaseAsset AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseAsset
  :: AssetId
  -> CardCode
  -> (AssetAttrs -> AssetAttrs)
  -> (CardDef -> CardDef)
  -> Asset
baseAsset aid cardCode attrsF defF = BaseAsset' $ cbCardBuilder
  (assetWith BaseAsset (defF $ testCardDef AssetType cardCode) attrsF)
  aid

instance HasDamage Asset where
  getDamage a =
    let AssetAttrs {..} = toAttrs a in (assetHealthDamage, assetSanityDamage)

instance HasActions env BaseAsset where
  getActions iid window (BaseAsset attrs) = getActions iid window attrs

instance HasModifiersFor env BaseAsset

instance AssetRunner env => RunMessage env BaseAsset where
  runMessage msg (BaseAsset attrs) = BaseAsset <$> runMessage msg attrs

instance Exhaustable Asset where
  isExhausted = assetExhausted . toAttrs

instance Discardable Asset where
  canBeDiscarded = assetCanLeavePlayByNormalMeans . toAttrs

instance HasId (Maybe OwnerId) env Asset where
  getId = pure . coerce . assetInvestigator . toAttrs

instance HasId (Maybe LocationId) env Asset where
  getId = pure . assetLocation . toAttrs

instance HasCount DoomCount env Asset where
  getCount = pure . DoomCount . assetDoom . toAttrs

instance HasCount ClueCount env Asset where
  getCount = pure . ClueCount . assetClues . toAttrs

instance HasCount UsesCount env Asset where
  getCount x = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (toAttrs x)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset cardCode =
  fromJustNote ("Unknown asset: " <> show cardCode) $ lookup cardCode allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  [ CardBuilder
    { cbCardCode = "asset"
    , cbCardBuilder = \aid -> baseAsset aid "asset" id id
    }
  , AbbessAllegriaDiBiase' <$> abbessAllegriaDiBiase
  , AdamLynch' <$> adamLynch
  , Adaptable1' <$> adaptable1
  , AlchemicalConcoction' <$> alchemicalConcoction
  , AlyssaGraham' <$> alyssaGraham
  , Aquinnah1' <$> aquinnah1
  , Aquinnah3' <$> aquinnah3
  , ArcaneEnlightenment' <$> arcaneEnlightenment
  , ArcaneInitiate' <$> arcaneInitiate
  , ArcaneStudies' <$> arcaneStudies
  , ArcaneStudies2' <$> arcaneStudies2
  , ArtStudent' <$> artStudent
  , Bandolier' <$> bandolier
  , BaseballBat' <$> baseballBat
  , BearTrap' <$> bearTrap
  , BeatCop' <$> beatCop
  , BeatCop2' <$> beatCop2
  , Blackjack' <$> blackjack
  , BloodPact3' <$> bloodPact3
  , BookOfShadows3' <$> bookOfShadows3
  , BrotherXavier1' <$> brotherXavier1
  , BulletproofVest3' <$> bulletproofVest3
  , Burglary' <$> burglary
  , CatBurglar1' <$> catBurglar1
  , CelaenoFragments' <$> celaenoFragments
  , Charisma3' <$> charisma3
  , ChicagoTypewriter4' <$> chicagoTypewriter4
  , ClarityOfMind' <$> clarityOfMind
  , DaisysToteBag' <$> daisysToteBag
  , DaisysToteBagAdvanced' <$> daisysToteBagAdvanced
  , DarkHorse' <$> darkHorse
  , DigDeep' <$> digDeep
  , DigDeep2' <$> digDeep2
  , DiscOfItzamna2' <$> discOfItzamna2
  , DrFrancisMorgan' <$> drFrancisMorgan
  , DrHenryArmitage' <$> drHenryArmitage
  , DrMilanChristopher' <$> drMilanChristopher
  , DrWilliamTMaleson' <$> drWilliamTMaleson
  , Duke' <$> duke
  , EarlSawyer' <$> earlSawyer
  , ElderSignAmulet3' <$> elderSignAmulet3
  , Encyclopedia' <$> encyclopedia
  , Encyclopedia2' <$> encyclopedia2
  , EsotericFormula' <$> esotericFormula
  , FineClothes' <$> fineClothes
  , FireAxe' <$> fireAxe
  , FireExtinguisher1' <$> fireExtinguisher1
  , FirstAid' <$> firstAid
  , FishingNet' <$> fishingNet
  , Flashlight' <$> flashlight
  , ForbiddenKnowledge' <$> forbiddenKnowledge
  , FortyFiveAutomatic' <$> fortyFiveAutomatic
  , FortyOneDerringer' <$> fortyOneDerringer
  , GrotesqueStatue4' <$> grotesqueStatue4
  , GuardDog' <$> guardDog
  , HardKnocks' <$> hardKnocks
  , HardKnocks2' <$> hardKnocks2
  , HaroldWalsted' <$> haroldWalsted
  , HeirloomOfHyperborea' <$> heirloomOfHyperborea
  , HelplessPassenger' <$> helplessPassenger
  , HigherEducation' <$> higherEducation
  , HigherEducation3' <$> higherEducation3
  , HiredMuscle1' <$> hiredMuscle1
  , HolyRosary' <$> holyRosary
  , Hyperawareness' <$> hyperawareness
  , Hyperawareness2' <$> hyperawareness2
  , InnocentReveler' <$> innocentReveler
  , JazzMulligan' <$> jazzMulligan
  , JennysTwin45s' <$> jennysTwin45s
  , JewelOfAureolus3' <$> jewelOfAureolus3
  , JimsTrumpet' <$> jimsTrumpet
  , JoeyTheRatVigil' <$> joeyTheRatVigil
  , KeenEye' <$> keenEye
  , KeenEye3' <$> keenEye3
  , KeyToTheChamber' <$> keyToTheChamber
  , Knife' <$> knife
  , Kukri' <$> kukri
  , LaboratoryAssistant' <$> laboratoryAssistant
  , LadyEsprit' <$> ladyEsprit
  , LeatherCoat' <$> leatherCoat
  , LeoDeLuca' <$> leoDeLuca
  , LeoDeLuca1' <$> leoDeLuca1
  , LightningGun5' <$> lightningGun5
  , LiquidCourage' <$> liquidCourage
  , LitaChantler' <$> litaChantler
  , LoneWolf' <$> loneWolf
  , LuckyDice2' <$> luckyDice2
  , Machete' <$> machete
  , MagnifyingGlass' <$> magnifyingGlass
  , MagnifyingGlass1' <$> magnifyingGlass1
  , MaskedCarnevaleGoer_17' <$> maskedCarnevaleGoer_17
  , MaskedCarnevaleGoer_18' <$> maskedCarnevaleGoer_18
  , MaskedCarnevaleGoer_19' <$> maskedCarnevaleGoer_19
  , MaskedCarnevaleGoer_20' <$> maskedCarnevaleGoer_20
  , MaskedCarnevaleGoer_21' <$> maskedCarnevaleGoer_21
  , MedicalTexts' <$> medicalTexts
  , MonstrousTransformation' <$> monstrousTransformation
  , Newspaper' <$> newspaper
  , OccultLexicon' <$> occultLexicon
  , OldBookOfLore' <$> oldBookOfLore
  , Painkillers' <$> painkillers
  , Pathfinder1' <$> pathfinder1
  , PeterClover' <$> peterClover
  , PeterSylvestre' <$> peterSylvestre
  , PeterSylvestre2' <$> peterSylvestre2
  , PhysicalTraining' <$> physicalTraining
  , PhysicalTraining2' <$> physicalTraining2
  , Pickpocketing' <$> pickpoketing
  , PoliceBadge2' <$> policeBadge2
  , PowderOfIbnGhazi' <$> powderOfIbnGhazi
  , ProfessorWarrenRice' <$> professorWarrenRice
  , RabbitsFoot' <$> rabbitsFoot
  , RabbitsFoot3' <$> rabbitsFoot3
  , RelicHunter3' <$> relicHunter3
  , ResearchLibrarian' <$> researchLibrarian
  , RiteOfSeeking' <$> riteOfSeeking
  , RiteOfSeeking4' <$> riteOfSeeking4
  , RitualCandles' <$> ritualCandles
  , Rolands38Special' <$> rolands38Special
  , Scavenging' <$> scavenging
  , Scrapper3' <$> scrapper3
  , ScrollOfProphecies' <$> scrollOfProphecies
  , Scrying' <$> scrying
  , Shotgun4' <$> shotgun4
  , Shrivelling' <$> shrivelling
  , Shrivelling3' <$> shrivelling3
  , Shrivelling5' <$> shrivelling5
  , SmokingPipe' <$> smokingPipe
  , SongOfTheDead2' <$> songOfTheDead2
  , SpringfieldM19034' <$> springfieldM19034
  , StrangeSolution' <$> strangeSolution
  , StrangeSolutionAcidicIchor4' <$> strangeSolutionAcidicIchor4
  , StrangeSolutionFreezingVariant4' <$> strangeSolutionFreezingVariant4
  , StrangeSolutionRestorativeConcoction4'
    <$> strangeSolutionRestorativeConcoction4
  , StrayCat' <$> strayCat
  , Streetwise3' <$> streetwise3
  , Switchblade' <$> switchblade
  , Switchblade2' <$> switchblade2
  , TheGoldPocketWatch4' <$> theGoldPocketWatch4
  , TheNecronomicon' <$> theNecronomicon
  , TheNecronomiconAdvanced' <$> theNecronomiconAdvanced
  , TheNecronomiconOlausWormiusTranslation'
    <$> theNecronomiconOlausWormiusTranslation
  , TheRedGlovedMan5' <$> theRedGlovedMan5
  , ToothOfEztli' <$> toothOfEztli
  , TryAndTryAgain3' <$> tryAndTryAgain3
  , WendysAmulet' <$> wendysAmulet
  , WhittonGreene' <$> whittonGreene
  , ZebulonWhateley' <$> zebulonWhateley
  , ZoeysCross' <$> zoeysCross
  ]

instance IsAsset Asset where
  slotsOf = slotsOf . toAttrs
  useTypeOf = useTypeOf . toAttrs
  isHealthDamageable = isHealthDamageable . toAttrs
  isSanityDamageable = isSanityDamageable . toAttrs
  isStory = isStory . toAttrs
